package zio.config.magnolia

import scala.quoted.*
import zio.config.derivation._

private[magnolia] object AnnotationMacros:
  inline def nameOf[T]: List[name]                                  = ${ filterAnnotations[T, name] }
  inline def discriminatorOf[T]: List[discriminator]                = ${ filterAnnotations[T, discriminator] }
  inline def descriptionOf[T]: List[describe]                       = ${ filterAnnotations[T, describe] }
  inline def caseModifier[T]: List[kebabCase | snakeCase]           = ${ filterAnnotations[T, kebabCase | snakeCase] }
  inline def kebabCaseOf[T]: List[kebabCase]                        = ${ filterAnnotations[T, kebabCase] }
  inline def snakeCaseOf[T]: List[snakeCase]                        = ${ filterAnnotations[T, snakeCase] }
  inline def keyModifiers[T]: List[prefix | postfix | suffix]       = ${ filterAnnotations[T, prefix | postfix | suffix] }
  inline def prefixOf[T]: List[prefix]                              = ${ filterAnnotations[T, prefix] }
  inline def postfixOf[T]: List[postfix]                            = ${ filterAnnotations[T, postfix] }
  inline def suffixOf[T]: List[suffix]                              = ${ filterAnnotations[T, suffix] }
  inline def fieldNamesOf[T]: List[(String, List[name])]            = ${ filterFieldAnnotations[T, name] }
  inline def fieldDescriptionsOf[T]: List[(String, List[describe])] = ${
    filterFieldAnnotations[T, describe]
  }

  private def filterAnnotations[T: Type, A: Type](using Quotes): Expr[List[A]] = {
    import quotes.reflect.*

    val annotationTpe = TypeRepr.of[A]

    def subTypeOf(tpe: TypeRepr, parent: TypeRepr): Boolean =
      tpe.simplified.dealias match
        case o: OrType =>
          subTypeOf(o.left, parent) || subTypeOf(o.right, parent)
        case o         =>
          tpe <:< parent

    val annotations = TypeRepr
      .of[T]
      .typeSymbol
      .annotations
      .collect:
        case term if subTypeOf(term.tpe, annotationTpe) => term

    Expr.ofList(annotations.reverse.map(_.asExprOf[A]))
  }

  private def filterFieldAnnotations[T: Type, A: Type](using Quotes): Expr[List[(String, List[A])]] =
    import quotes.reflect.*

    val annotationTpe = TypeRepr.of[A]

    val namedAnnotations = TypeRepr
      .of[T]
      .typeSymbol
      .primaryConstructor
      .paramSymss
      .flatten
      .map(field => field.name -> field.annotations)

    Expr
      .ofList(
        namedAnnotations
          .map:
            case (name, terms) =>
              name -> terms.collect:
                case term if term.tpe =:= annotationTpe => term
          .map:
            case (name, terms) => Expr(name) -> terms.reverse.map(_.asExprOf[A])
          .map((name, annotations) => Expr.ofTuple((name, Expr.ofList(annotations))))
      )
end AnnotationMacros
