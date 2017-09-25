//=============================================================================
// lang/ContextMutationSet.scala : CaSCAS Project
//=============================================================================
// A ContextMutationSet represents a collection of potential changes to be
// applied to a Context.

package org.cascas_project.cascas.lang

//=============================================================================

import org.cascas_project.cascas.lang.liro.Identifier

import scala.collection.immutable.Map
import scala.collection.mutable.{Map => MMap}

//=============================================================================

class ContextMutationSet {

  private var introductions: MMap[Identifier, TypeIdentifier] = MMap()
  private var assignments:   MMap[Identifier, TypedObject] = MMap()
  private var reassignments: MMap[Identifier, TypedObject] = MMap()

  def withIntroductions(
    intros: MMap[Identifier, TypeIdentifier]
  ): ContextMutationSet = {
    this.introductions ++= intros
    this
  }

  def withIntroductions(
    intros: Vector[FormalParameter]
  ): ContextMutationSet = {
    this.introductions ++= intros.map(x => (x.id, x.tpe)).toMap
    this
  }

  def withAssignments(
    assigns: MMap[Identifier, TypedObject]
  ): ContextMutationSet = {
    this.assignments ++= assigns
    this
  }

  def withReassignments(
    reassigns: MMap[Identifier, TypedObject],
    ctx:       Context
  ): ContextMutationSet = {
    reassigns.foreach {
      case (id, TypedObject(tpe, obj)) => {
        this.reassign(id, obj, ctx)
      }
    }
    this
  }

  private def withReassignments(
    reassigns: MMap[Identifier, TypedObject]
  ): ContextMutationSet = {
    this.reassignments ++= reassigns
    this
  }

  override def clone(): ContextMutationSet = {
    (
      new ContextMutationSet
    ).withIntroductions(
      this.introductions
    ).withAssignments(
      this.assignments
    ).withReassignments(
      this.reassignments
    )
  }

  def getIntroductions: Map[Identifier, TypeIdentifier] = this.introductions.toMap
  def getAssignments:   Map[Identifier, TypedObject]    = this.assignments.toMap
  def getReassignments: Map[Identifier, TypedObject]    = this.reassignments.toMap

  def introduce(id: Identifier, tpe: TypeIdentifier): ContextMutationSet = {
    this.introductions += (id -> tpe)
    this
  }

  def assign(id: Identifier, obj: liro.Object, ctx: Context): ContextMutationSet = {
    obj.inferType(ctx) match {
      case Some(tpe) => {
        this.assignments += (id -> TypedObject(tpe, obj))
      }
      case None => {
        throw new Exception("Cannot infer type of obj in ctx") //TODO
      }
    }
    this
  }

  def assign(id: Identifier, tpedObj: TypedObject): ContextMutationSet = {
    this.assignments += (id -> tpedObj)
    this
  }

  def reassign(id: Identifier, obj: liro.Object, ctx: Context): ContextMutationSet = {
    ctx.get(id) match {
      // Reassignment of an assigned identifier
      case Some(TypedObject(oldTpe, oldObj)) => {
        obj.inferType(ctx) match {
          case Some(tpe) if (tpe == oldTpe) => {
            this.reassignments += (id -> TypedObject(oldTpe, obj))
            this
          }
          case Some(other) => {
            throw new Exception("Type mismatch on reassignment") //TODO
          }
          case None => {
            throw new Exception("Cannot infer type of obj") //TODO
          }
        }
      }
      // Reassignment of an introduced (unassigned) identifier
      case Some(oldTpe: TypeIdentifier) => {
        obj.inferType(ctx) match {
          case Some(tpe) if tpe == oldTpe => {
            this.reassignments += (id -> TypedObject(oldTpe, obj))
            this
          }
          case Some(other) => {
            throw new Exception("Type mismatch on reassignment") //TODO
          }
          case None => {
            throw new Exception("Cannot infer type of obj") //TODO
          }
        }
      }
      // Not really possible
      case Some(other) => {
        throw new Exception("Not possible damn it") //TODO
      }
      // Reassignment of a non-introduced identifier
      case None => {
        throw new Exception("Cannot reassign non-introduced identifier") //TODO
      }
    }
  }

  def onlyIntroductions(): ContextMutationSet = {
    var res = new ContextMutationSet
    res.introductions = this.introductions
    res
  }
  
  def onlyAssignments(): ContextMutationSet = {
    var res = new ContextMutationSet
    res.assignments = this.assignments
    res
  }

  def onlyReassignments(): ContextMutationSet = {
    var res = new ContextMutationSet
    res.reassignments = this.reassignments
    res
  }

  def ++(that: ContextMutationSet): ContextMutationSet = {
    this.clone().withIntroductions(
      that.introductions
    ).withAssignments(
      that.assignments
    ).withReassignments(
      that.reassignments
    )
  }

  override def toString: String = {
    "Introductions:" + ( if (this.introductions.isEmpty) "<<NONE>>\n" else {
      this.introductions.mkString("\n    ", "\n    ", "\n")
    }) +
    "Assignments:" + ( if (this.assignments.isEmpty) "<<NONE>>\n" else {
      this.assignments.mkString("\n    ", "\n    ", "\n")
    }) +
    "Reassignments:" + ( if (this.reassignments.isEmpty) "<<NONE>>\n" else {
      this.reassignments.mkString("\n    ", "\n    ", "")
    })
  }
}

object ContextMutationSet {

  def empty = new ContextMutationSet

}
