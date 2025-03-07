using Igor.Erlang;
using Igor.Erlang.Model;
using Igor.Erlang.AST;
using Igor;

public class CatalogueGenerator : IErlangGenerator
{
    static string func = @"
-spec {0}(Key) -> Record when
      Key :: catalogue:key(),
      Record :: {1}.

{0}(Key) -> 
    catalogue:view(Key, {0}, {2}).
";

    static string funcWithDb = @"
-spec {0}(Db, Key) -> Record when
      Db :: atom(),
      Key :: catalogue:key(),
      Record :: {1}.

{0}(Db, Key) -> 
    catalogue:view(Db, Key, {0}, {2}).
";

    static string isTypeOfFFunc = @"
-spec is_{0}(Key) -> boolean() when
      Key :: catalogue:key() | {1}.

is_{0}(Key) when is_atom(Key) -> 
    element(1, card(Key)) =:= {0};
is_{0}(#{0}{{}}) ->
    true;
is_{0}(_) -> 
    false.
";

    public static readonly BoolAttributeDescriptor CatalogueEnabledAttribute = new BoolAttributeDescriptor("catalogue.enabled", IgorAttributeTargets.Any, AttributeInheritance.Scope);
    public static readonly StringAttributeDescriptor CatalogueFileAttribute = new StringAttributeDescriptor("catalogue.file", IgorAttributeTargets.Module);
    public static readonly BoolAttributeDescriptor CatalogueTypeOfEnabledAttribute = new BoolAttributeDescriptor("catalogue.type_of_enabled", IgorAttributeTargets.Any, AttributeInheritance.Scope);
    public static readonly BoolAttributeDescriptor CatalogueMultiDbAttribute = new BoolAttributeDescriptor("catalogue.multi_db", IgorAttributeTargets.Module);

    public void Generate(ErlModel model, Module module)
    {
        foreach (var form in module.Structs)
        {
            var enabled = form.attributes.Attribute(CatalogueEnabledAttribute, false);
            
            if (enabled)
            {
                var view_file = module.attributes.Attribute(CatalogueFileAttribute, module.erlFileName);
                var useMultiDb = module.attributes.Attribute(CatalogueMultiDbAttribute, false);
                var erl = model.Module(view_file);
                erl.Include(module.hrlFileName);
                erl.Export(form.erlName, useMultiDb ? 2: 1);
                var funcTemplate = useMultiDb ? funcWithDb : func;
                erl.Function(string.Format(funcTemplate, form.erlName, form.erlRemoteType, Igor.Erlang.Json.JsonSerialization.ParseJsonFun(form.erlJsonTag(form), erl.Name)));

                if (!(form is VariantForm) && module.attributes.Attribute(CatalogueTypeOfEnabledAttribute, false))
                {
                    erl.Export("is_" + form.erlName, 1);
                    erl.Function(string.Format(isTypeOfFFunc, form.erlName, form.erlRemoteType, form.Ancestor.erlName));
                }
            }
        }
    }
}