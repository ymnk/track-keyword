package com.jcraft.track_keyword.comet

import _root_.net.liftweb.http._
import S._
import SHtml._
import _root_.net.liftweb.util._
import _root_.scala.xml._
 
class AskKeyword extends CometActor {
  def render = {
    val tracked = TrackTwitter.keywords.toList match{
      case Nil => Text("") 
      case l => 
        <div>Current tracked keyword: {l.mkString(",")}</div>
    }
    ajaxForm(tracked ++
             text("",name => answer(name.trim)) ++
             <input type="submit" value="Track It"/>)
  }
}
