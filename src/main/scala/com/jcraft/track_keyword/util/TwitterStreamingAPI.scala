package com.jcraft.track_keyword.util

import _root_.scala.xml.{XML, Elem}
//import _root_.net.liftweb.util.SecurityHelpers
import _root_.org.apache.commons.codec.binary.Base64


class Http{
  import java.net.URLEncoder.encode
  import java.net._
  import java.net.{URL, HttpURLConnection}
  import java.io.{InputStream, IOException}

  private var basicAuth:Option[String] = None

  def this(auth:Option[(String,String)]){
    this()
    auth.map{
      case (user, passwd) =>
      basicAuth = Some("Basic "+new String((new Base64).encode((user+":"+passwd).getBytes)))
//    basicAuth = Some("Basic "+SecurityHelpers.base64Encode((user+":"+passwd).getBytes))
    }
  }

  private var in:Option[InputStream] = None
 
  private def param2str(param:(String,String)*):String =
    (for((k, v)<-param)
       yield k+"="+URLEncoder.encode(v, "UTF-8")).mkString("&")

  private def consume(_in:InputStream, f:Option[String]=>Unit){
    in = Some(_in)
    val buf = new Array[Byte](1024)
    try{
      for(i <- Stream.const(()=>_in.read(buf)).map(_()).takeWhile(_ != -1)){
        f(Some(new String(buf, 0, i)))
      }
      f(None)
    }
    catch{ case e:IOException => }
    finally{ _in.close }
  }

  def get(uri:String, param:(String,String)*)(f: Option[String]=>Unit) = {
    new URL(uri + "?" + param2str(param:_*)).openConnection match{
      case c:HttpURLConnection =>
        c.setRequestMethod("GET")
        basicAuth.map(b => c.setRequestProperty("Authorization", b))
        consume(c.getInputStream, f)
      case _ =>
    }
  }

  def post(uri:String, param:(String,String)*)(f: Option[String]=>Unit)={
    new URL(uri).openConnection match{
      case c:HttpURLConnection => {
        c.setDoInput(true)
        c.setDoOutput(true)
        c.setUseCaches(false)
        c.setRequestMethod("POST")
        c.setRequestProperty("Content-Type",
                             "application/x-www-form-urlencoded")
        basicAuth.map(b => c.setRequestProperty("Authorization", b))
 
        val content = param2str(param:_*).getBytes
        c.setRequestProperty("Content-Length", content.length.toString);
        val o = c.getOutputStream
        o.write(content)
        o.flush
        o.close

        consume(c.getInputStream, f)
      }
      case _ => None
    }
  }

  def disconnect(){
    try{ in.map(_.close) } catch { case e => }
  } 
}

class TwitterStreamingAPI{
  import scala.collection.mutable.{Queue, SynchronizedQueue}

  private var auth:Option[(String,String)] = None
  def this(user:String, passwd:String) = {
    this()
    auth = Some(user -> passwd)
  }

  private val follow = "http://stream.twitter.com/follow.xml"
  private val spritzer = "http://stream.twitter.com/spritzer.xml"
  private val track = "http://stream.twitter.com/track.xml"

  private var http:Option[Http] = None

  // ?s will enable the DOTALL mode
  // *? is the reluctant quantifier, and not greedy.
  private val pattern1 ="^((?s).*?)</limit>((?s).*)".r
  private val pattern2 ="^((?s).*?)</status>((?s).*)".r
  private def parseStatus(queue:Queue[Elem]) = {
    var input = ""
    val proc:Option[String]=>Unit = {
      case Some(_input) =>
        input = (input + _input) match{
          case pattern1(_, y) => y
          case pattern2(x, y) =>
            queue += XML.loadString(x.trim+"</status>")
            y
          case _input => _input
        }
      case _ => 
    }
    proc
  }

  private def spawnQueueReader(f:(Elem) => Unit):Queue[Elem]={
    val queue = new SynchronizedQueue[Elem]

    import scala.concurrent.ops.spawn
    spawn{
      def loop:Unit = queue.dequeueFirst((_)=>true) match{
        case Some(e) => f(e); loop
        case _ =>
      }

      while(!http.isEmpty){
        loop
        Thread.sleep(100)
      }
    }

    queue
  }

  def follow(id:Seq[String])(f:(Elem) => Unit){
    http=Some(new Http(auth))
    val queue = spawnQueueReader(f)
    val _id = id.take(200).mkString(" ")
    http.map(_.post(follow, ("follow", _id))(parseStatus(queue)))
  }

  def spritzer(f:(Elem) => Unit){
    http=Some(new Http(auth))
    val queue = spawnQueueReader(f)
    http.map(_.get(spritzer)(parseStatus(queue)))
  }

  def track(keyword:Seq[String])(f:(Elem) => Unit){
    http=Some(new Http(auth))
    val queue = spawnQueueReader(f)
    val _keyword = keyword.take(50).mkString(",")
    http.map(_.post(track, ("track", _keyword))(parseStatus(queue)))
  }

  def disconnect(){
    http.map(_.disconnect)
    http = None
  }
}

/*
object RT {

  def main(arg:Array[String]){
    import java.net.{Authenticator, PasswordAuthentication}
    val (username, passwd) = (arg(0), arg(1))
    Authenticator.setDefault(
      new Authenticator {
        override def getPasswordAuthentication = {
          new PasswordAuthentication(username, passwd.toCharArray);
        }
      }
    )

    TwitterStreamingAPI.spritzer {
      case e =>
        var (text, user_name) = (e \ "text" text, e \ "user" \ "name" text)
        if(text.indexOf("RT ") != -1)
          println(user_name+": "+text)
    }
  }
}


object UpdatesOfFollowers {

  def main(arg:Array[String]){
    import java.net.{Authenticator, PasswordAuthentication}
    val (username, passwd) = (arg(0), arg(1))
    Authenticator.setDefault(
      new Authenticator {
        override def getPasswordAuthentication = {
          new PasswordAuthentication(username, passwd.toCharArray);
        }
      }
    )

    val followers = new FriendOrFollow(username).followers.keys.collect

    TwitterStreamingAPI.follow(followers) {
      case e =>
        var (text, user_name) = (e \ "text" text, e \ "user" \ "name" text)
        println(user_name+": "+text)
    }
  }
}
*/

object Track {
  def main(arg:Array[String]){
    import java.net.{Authenticator, PasswordAuthentication}
    val Array(username, passwd, rest@_*) = arg
/*
    Authenticator.setDefault(
      new Authenticator {
        override def getPasswordAuthentication = {
          new PasswordAuthentication(username, passwd.toCharArray);
        }
      }
    )
*/
   (new TwitterStreamingAPI(username, passwd)).track(rest) {
      case e =>
        var (id,
             text, 
             user_name,
             user_profile_image_url) = (e \ "id" text,
                           e \ "text" text, 
                           e \ "user" \ "name" text,
                           e \ "user" \ "profile_image_url" text)
        println(user_name+": "+text)
    }
  }
}
