package com.jcraft.track_keyword.comet

import _root_.scala.actors._
import Actor._
import _root_.net.liftweb._
import http._
import util._
import Helpers._
import _root_.scala.xml._
import S._
import SHtml._
import js._
import JsCmds._
import JE._
import net.liftweb.http.js.jquery.JqJsCmds._
 
class Track extends CometActor with CometListener {
  private var keyword = ""
  private var status: List[StatusLine] = Nil
  private lazy val infoId = uniqueId + "_info"
  private lazy val infoIn = uniqueId + "_in"
  private lazy val inputArea = findKids(defaultXml, "track", "input")
  private lazy val bodyArea = findKids(defaultXml, "track", "body")
  private lazy val singleLine = deepFindKids(bodyArea, "track", "list")
 
  override def lowPriority = {
    case StatusUpdate(_value) if keyword.length != 0 =>
      val value = _value.filter{
        case b@StatusLine(_, msg, _, _) => 
          msg.toString.toLowerCase.indexOf(keyword.toLowerCase)>=0
      }
      val update = (value -- status).map{b => PrependHtml(infoId, line(b))}
      partialUpdate(update)
      status = value
  }

  override lazy val fixedRender: Box[NodeSeq] =
    ajaxForm(After(100, SetValueAndFocus(infoIn, "")),
             bind("track", inputArea,
                  "input" -> text(keyword, 
                                  (k) => {
                                    keyword = k
                                    status = Nil
                                    sendMessage(k)
                                    reRender(true)
                                  }, 
                                  "id" -> infoIn)))
 
  private def sendMessage(msg: String) = TrackServer ! TrackKeyword(keyword)
 
  private def line(c: StatusLine) = 
    bind("list", singleLine,
         "when" -> c.when,
         "img" -> <img src={c.img} style="width:48px; height:48px" /> ,
         "who" -> c.user,
         "msg" -> c.msg)
 
  private def displayList(in: NodeSeq): NodeSeq = status.flatMap(line)
 
  override def render =
    bind("track", bodyArea,
         "name" -> keyword,
         AttrBindParam("id", Text(infoId), "id"),
         "list" -> displayList _)
 
  override def localSetup {
    askForKeyword
    super.localSetup
  }
 
  def registerWith = TrackServer
 
  private def askForKeyword {
    if (keyword.length == 0) {
      ask(new AskKeyword, "keyword to be tracked") {
        case s: String if (s.trim.length > 2) =>
          keyword = s.trim
          status = Nil
          sendMessage(keyword)
          reRender(true)
        case _ =>
          askForKeyword
          reRender(false)
      }
    }
  }
 
}
 
