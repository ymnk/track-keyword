package com.jcraft.track_keyword.comet

import _root_.scala.actors.Actor
import Actor._
import _root_.net.liftweb._
import http._
import util._
import Helpers._
import _root_.scala.xml.{NodeSeq, Text}
import textile.TextileParser
import _root_.java.util.Date
import _root_.scala.collection.mutable.Set
import _root_.scala.concurrent.ops.spawn

import _root_.com.jcraft.track_keyword.util._
 
object TrackServer extends Actor with ListenerManager {
  private var status: List[StatusLine] = Nil
 
  override def lowPriority = {
    case e@TrackKeyword(keyword) => TrackTwitter ! e
    case Status(user, msg, img, when) =>
      status ::= StatusLine(user, toHtml(msg), img, when)
      status = status.take(50)
      updateListeners()
    case _ =>
  }
 
  def createUpdate = StatusUpdate(status.take(15))
 
  def toHtml(msg: String): NodeSeq = 
    TextileParser.paraFixer(TextileParser.toHtml(msg, Empty))
 
  this.start
}
 
case class TrackKeyword(keyword: String)
case class Status(user: String, msg: String, img:String, when:String)
case class StatusLine(user: String, msg: NodeSeq, img:String, when:String)
case class StatusUpdate(msgs: List[StatusLine])

object TrackTwitter extends Actor {
  var keywords: Set[String] = Set.empty[String]
  private var track:Option[TwitterStreamingAPI] = None

  val (username, passwd) = ("XXX", "XXX")

  def act = {
    loop { react {
      case TrackKeyword(keyword) => {
        keywords += keyword
        track.map(_.disconnect)
        track = Some(new TwitterStreamingAPI(username, passwd))

        spawn{ 
          track.map(_.track(keywords.toList){
            case e =>
            var (id, text, when, user_name, user_profile_image_url) = 
                (e \ "id" text,
                 e \ "text" text,
                 e \ "created_at" text,
                 e \ "user" \ "name" text,
                 e \ "user" \ "profile_image_url" text)
            TrackServer ! Status(user_name, text, user_profile_image_url, when)
          })
        }
      }
      case _ =>
    }}
  }

  this.start
}
