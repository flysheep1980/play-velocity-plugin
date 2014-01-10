/*
 * Copyright (C) 2013 FURYU CORPORATION
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Includes Apache Velocity
 *
 *   http://velocity.apache.org/
 *
 * Copyright (C) 2000-2007 The Apache Software Foundation
 */
package jp.furyu.play.velocity

import java.io.{ Writer, StringWriter }
import java.util.{ Iterator => JavaIterator, Properties }

import org.apache.velocity.app.VelocityEngine
import org.apache.velocity.runtime.log.Log
import org.apache.velocity.runtime.parser.node.{ Node, MapGetExecutor, PropertyExecutor }
import org.apache.velocity.util.introspection.UberspectImpl.{ VelMethodImpl, VelGetterImpl }
import org.apache.velocity.util.introspection._
import org.apache.velocity.VelocityContext

import play.api.templates.Html
import play.api.Application
import play.api.Plugin
import org.apache.velocity.runtime.directive.{ DirectiveConstants, Directive }
import org.apache.velocity.context.InternalContextAdapter
import org.apache.velocity.exception.VelocityException

/**
 * Velocity Plugin for Play2!
 *
 * @see http://www.frothandjava.com/2010/02/scala-spring-templates-velocity.html
 */
class VelocityPlugin(app: Application) extends Plugin {

  private lazy val logger = play.api.Logger("jp.furyu.play.velocity.VelocityPlugin")
  private val VelocityPluginRuntimeProperties = "velocity_plugin.properties"

  lazy val engine: VelocityEngine = {
    // initialize velocity engine
    val prop = new Properties
    val is = this.getClass().getResourceAsStream("/" + VelocityPluginRuntimeProperties)
    if (is != null) {
      logger.info("setup engine in [%s]".format(VelocityPluginRuntimeProperties))
      prop.load(is)
    }

    val engine = new VelocityEngine(prop)
    engine.init

    engine
  }

  override def onStart() {
    logger.info("initialize engine")
    engine
  }

  override val enabled: Boolean = true
}

package object mvc {

  /**
   * marge velocity template to Html.
   *
   * @param templatePath relative path of template file to "file.resource.loader.path"
   * @param attributes request attributes (default empty)
   * @param charset encoding template charset (default utf-8)
   * @return Html
   * @throws ResourceNotFoundException not found template file
   * @throws ParseErrorException template invalid velocity format
   * @throws MethodInvocationException error occur when evaluate template in object of context
   */
  def VM(templatePath: String, attributes: Map[String, Any] = Map.empty, charset: String = "utf-8"): Html = {
    val plugin = play.api.Play.current.plugin[VelocityPlugin]
      .getOrElse(throw new IllegalStateException("VelocityPlugin not installed"))

    // create context and set attributes
    val context = new VelocityContext
    attributes.foreach { case (key, value) => context.put(key, value) }

    // evaluate template by velocity
    val writer = new StringWriter
    plugin.engine.mergeTemplate(templatePath, charset, context, writer)

    // wrap Html
    play.api.templates.HtmlFormat.raw(writer.toString)
  }
}

/**
 * Uberspect for Scala.
 *
 * <p>
 * Velocity uses introspection/reflection to access properties and methods on an object.<br>
 * Uberspect have the responsibility of it.<br>
 * This implementation of Uberspect customize for Scala.
 * </p>
 */
class ScalaUberspect extends UberspectImpl {

  import jp.furyu.play.velocity.ScalaUberspect.{ ScalaMapGetExecutor, ScalaPropertyExecutor }

  override def getIterator(obj: java.lang.Object, i: Info): JavaIterator[_] = {
    def makeJavaIterator(iter: Iterator[_]) = new JavaIterator[AnyRef] {
      override def hasNext() = iter.hasNext
      override def next() = iter.next().asInstanceOf[AnyRef]
      override def remove() = throw new java.lang.UnsupportedOperationException("Remove not supported")
    }

    obj match {
      case i: Iterable[_] => makeJavaIterator(i.iterator)
      case i: Iterator[_] => makeJavaIterator(i)
      case _ => super.getIterator(obj, i)
    }
  }

  override def getPropertyGet(obj: java.lang.Object, identifier: String, i: Info): VelPropertyGet = {
    if (obj != null) {
      val claz = obj.getClass()

      val executor = obj match {
        case m: Map[_, _] => new ScalaMapGetExecutor(log, claz, identifier)
        case _ => new ScalaPropertyExecutor(log, introspector, claz, identifier)
      }

      if (executor.isAlive) {
        new VelGetterImpl(executor)
      } else {
        super.getPropertyGet(obj, identifier, i)
      }
    } else {
      null
    }
  }

  override def getMethod(obj: scala.Any, methodName: String, args: Array[AnyRef], i: Info): VelMethod = {
    println("called getMethod. obj: [%s], methodName: [%s], args: [%s], info: [%s]".format(obj, methodName, args, i))

    val ret = super.getMethod(obj, methodName, args, i)
    if (ret == null) {
      obj match {
        case opt: Option[_] if methodName == "getOrElse" => {
          val method = obj.getClass.getMethods.find(_.getName == "getOrElse").head

          new VelMethodImpl(method) {
            override def invoke(o: scala.Any, actual: Array[AnyRef]): AnyRef = {
              val newActual = actual.map { act =>
                new Function0[AnyRef] {
                  def apply(): AnyRef = act
                }.asInstanceOf[AnyRef]
              }
              super.invoke(o, newActual)
            }
          }
        }
        case opt: Option[_] if methodName == "map" => {
          val method = obj.getClass.getMethods.find(_.getName == "map").head

          method.getParameterTypes.foreach { parameterType =>
            println("parameterType: [%s]".format(parameterType.getName))
          }

          def convertFunc(arg: AnyRef) = {
            val str = "val v: Long => Long = { %s };v".format(arg.toString)
            println("str: [%s]".format(str))
            new com.twitter.util.Eval().apply[Long => Long](str)
            //            new com.twitter.util.Eval().apply[Long => Long]("""{ %s }""".format(arg.toString))
          }

          new VelMethodImpl(method) {
            override def invoke(o: scala.Any, actual: Array[AnyRef]): AnyRef = {
              val newActual = actual.map { act =>
                convertFunc(act).asInstanceOf[AnyRef]
              }
              super.invoke(o, newActual)
            }
          }
        }
        case _ => null
      }
    } else {
      ret
    }
  }

}
object ScalaUberspect {

  private class ScalaPropertyExecutor(log: Log, introspector: Introspector, clazz: java.lang.Class[_], property: String) extends PropertyExecutor(log, introspector, clazz, property) {
    override def discover(clazz: java.lang.Class[_], property: String) = {
      setMethod(introspector.getMethod(clazz, property, Array[java.lang.Object]()))
      if (!isAlive()) {
        super.discover(clazz, property)
      }
    }
  }

  private class ScalaMapGetExecutor(val llog: Log, val clazz: java.lang.Class[_], val property: String) extends MapGetExecutor(llog, clazz, property) {
    override def isAlive = true
    override def execute(o: AnyRef) = o.asInstanceOf[Map[String, AnyRef]].getOrElse[AnyRef](property, null).asInstanceOf[java.lang.Object]
  }

}

class ScalaFunctionDirective extends Directive {
  // http://www.sergiy.ca/how-to-create-custom-directives-for-apache-velocity/
  def getName: String = "func"
  def getType: Int = DirectiveConstants.LINE
  def render(context: InternalContextAdapter, writer: Writer, node: Node): Boolean = {
    if (node.jjtGetNumChildren() != 1) {
      throw new VelocityException("#func(): argument size must be one at " + Log.formatFileString(this))
    }

    val value = node.jjtGetChild(0).value(context)
    if (rsvc.getLog().isDebugEnabled()) {
      rsvc.getLog().debug("#func(): argument at " + Log.formatFileString(this))
    }
    println("value: [%s]".format(value))

    val a: AnyRef => AnyRef = { _.toString }
    //    new Function1[AnyRef, AnyRef] {
    //      def apply(v1: AnyRef): AnyRef = { _.toString }
    //    }

    val str = "val v: AnyRef => AnyRef = { %s };v".format(value.toString)
    println("str: [%s]".format(str))
    val v = new com.twitter.util.Eval().apply[AnyRef => AnyRef](str)
    //    val v = new com.twitter.util.Eval().apply[Function[AnyRef, AnyRef]]("{ s => s.toString }")
    //    val v = new com.twitter.util.Eval().apply[Function[AnyRef, AnyRef]]("{ _.toString }")
    val ret = v.apply("10")
    println("ret: [%s]".format(ret))

    context.put("func1", v)

    /*
     longOpt : Some(10L)
     longOpt.map(_.toString).getOrElse("hoge")
       -> "10" or "hoge"

     Long => String


     */

    true
  }
}