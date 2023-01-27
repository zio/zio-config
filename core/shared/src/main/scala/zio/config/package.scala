package zio

import zio.config.syntax.ConfigSyntax

package object config
    extends KeyConversionFunctions
    with ConfigSyntax
    with ImplicitTupleConversion
    with ConfigDocsModule
