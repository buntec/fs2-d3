/*
 * Copyright 2022 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package d3

import com.raquo.domtypes.generic.builders.StyleBuilders
import com.raquo.domtypes.generic.builders.canonical.CanonicalHtmlAttrBuilder
import com.raquo.domtypes.generic.builders.canonical.CanonicalPropBuilder
import com.raquo.domtypes.generic.builders.canonical.CanonicalReflectedHtmlAttrBuilder
import com.raquo.domtypes.generic.builders.canonical.CanonicalSvgAttrBuilder
import com.raquo.domtypes.generic.defs.attrs.HtmlAttrs
import com.raquo.domtypes.generic.defs.attrs.SvgAttrs
import com.raquo.domtypes.generic.defs.complex.canonical.CanonicalComplexHtmlKeys
import com.raquo.domtypes.generic.defs.reflectedAttrs.ReflectedHtmlAttrs
import com.raquo.domtypes.generic.defs.styles.Styles
import com.raquo.domtypes.generic.defs.styles.Styles2
import com.raquo.domtypes.generic.keys.HtmlAttr
import com.raquo.domtypes.generic.keys.Prop
import com.raquo.domtypes.generic.keys.Style
import com.raquo.domtypes.generic.keys.SvgAttr

object syntax {

  object html extends HtmlAttrsSyntax with StylesSyntax

  object svg extends SvgAttrsSyntax

  class StyleSetter private[syntax] (
      val name: String,
      val value: Option[String]
  )

  trait StylesSyntax
      extends Styles[StyleSetter]
      with Styles2[StyleSetter]
      with StyleBuilders[StyleSetter] {

    implicit class StyleOps[V](style: Style[V]) {

      def remove: StyleSetter = new StyleSetter(style.name, None)

      def :=(value: V): StyleSetter =
        new StyleSetter(style.name, Some(value.toString))

    }

    override protected def buildIntStyleSetter(
        style: Style[Int],
        value: Int
    ): StyleSetter = new StyleSetter(style.name, Some(value.toString))

    override protected def buildDoubleStyleSetter(
        style: Style[Double],
        value: Double
    ): StyleSetter = new StyleSetter(style.name, Some(value.toString))

    override protected def buildStringStyleSetter(
        style: Style[_],
        value: String
    ): StyleSetter = new StyleSetter(style.name, Some(value.toString))

  }

  trait HtmlAttrsSyntax
      extends HtmlAttrs[HtmlAttr]
      with ReflectedHtmlAttrs[CanonicalReflectedHtmlAttrBuilder.ReflectedAttr]
      with CanonicalComplexHtmlKeys[
        CanonicalReflectedHtmlAttrBuilder.ReflectedAttr,
        HtmlAttr,
        Prop
      ]
      with CanonicalHtmlAttrBuilder
      with CanonicalReflectedHtmlAttrBuilder
      with CanonicalPropBuilder

  trait SvgAttrsSyntax extends SvgAttrs[SvgAttr] with CanonicalSvgAttrBuilder

}
