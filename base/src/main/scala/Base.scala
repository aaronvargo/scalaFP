package scalaFP

trait Base
    extends BaseHierarchy
    with HasModule
    with FunctorModule with ApplyModule with ApplicativeModule with BindModule
    with NaperianModule
    with ProfunctorModule with StrongModule with ChoiceModule with ClosedModule
    with IdentityModule with ConstModule
    with Functor1Module
    with AppliedModule
    with OpticModule
    with StdModule {
}
