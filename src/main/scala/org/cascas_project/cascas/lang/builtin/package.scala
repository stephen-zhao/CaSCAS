//=============================================================================
// lang/builtin/package.scala : CaSCAS Project
//=============================================================================

package org.cascas_project.cascas.lang

//=============================================================================

import org.cascas_project.cascas.lang.liro.Identifier

//=============================================================================

package object builtin {

  // The context mutation set to add the built-in definitions to the built-in context
  private var builtInContextMutationSet: ContextMutationSet = ContextMutationSet.empty

  // List the built-ins to be included in the language:

  // Introduce the Primitive Types
  builtInContextMutationSet.introduce(Identifier("Number"),Identifier("Type"))
  builtInContextMutationSet.introduce(Identifier("Bool"),Identifier("Type"))

  // Assign the Type-Generics
  builtInContextMutationSet.assign(ListOperator.ident, ListOperator())

  // Assign the built-in operators
  builtInContextMutationSet.assign(AdditionOperator.ident, AdditionOperator())
  builtInContextMutationSet.assign(MultiplyOperator.ident, MultiplyOperator())

  // To add another entry to the list, do the following
  // e.g.
  //builtInContextMutationSet.assign(Identifier("...."), .....Operator())

  // Export the built-ins as a context
  val builtInCtx: Context = Context() :+ builtInContextMutationSet

}
