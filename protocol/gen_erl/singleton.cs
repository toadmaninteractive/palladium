using Igor.Erlang;
using Igor.Erlang.Model;
using Igor.Erlang.AST;
using Igor;

public class SingletonGenerator : IErlangGenerator
{
    const string func = @"
-spec {0}() -> {1}.

{0}() -> 
    ?{2}#{3}.{0}.
";

    const string funcWithDb = @"
-spec {0}(Db :: atom()) -> {1}.

{0}(Db) -> 
    ?{2}(Db)#{3}.{0}.
";

    public static readonly StringAttributeDescriptor SingletonNameAttribute = new StringAttributeDescriptor("singleton.name", IgorAttributeTargets.Record);
    public static readonly StringAttributeDescriptor SingletonFileAttribute = new StringAttributeDescriptor("singleton.file", IgorAttributeTargets.Record);
    public static readonly StringAttributeDescriptor CatalogueFileAttribute = new StringAttributeDescriptor("catalogue.file", IgorAttributeTargets.Module);
    public static readonly BoolAttributeDescriptor CatalogueMultiDbAttribute = new BoolAttributeDescriptor("catalogue.multi_db", IgorAttributeTargets.Module);

    public void Generate(ErlModel model, Module module)
    {
        var useMultiDb = module.attributes.Attribute(CatalogueMultiDbAttribute, false);

        foreach (var record in module.Records)
        {
            var name = record.attributes.Attribute(SingletonNameAttribute);
            if (name != null)
            {
                var file = record.attributes.Attribute(SingletonFileAttribute, module.erlFileName);
                var erl = model.Module(file);
                erl.Include(module.hrlFileName);
                var catalogue = module.attributes.Attribute(CatalogueFileAttribute, module.erlFileName);
                var macroName = useMultiDb ? name + "(Db)" : name;
                var macroFmt = useMultiDb ? "({0}:{1}(Db, {2}))" : "({0}:{1}({2}))";
                erl.Define(macroName, string.Format(macroFmt, catalogue, record.erlName, name));

                foreach (var field in record.Fields)
                    if (!field.IsTag && field.Name != "id")
                    {
                        erl.Export(field.erlName, useMultiDb ? 1 : 0);
                        erl.Function(string.Format(useMultiDb ? funcWithDb : func, field.erlName, field.erlType, name, record.erlName));
                    }
            }
        }
    }
}