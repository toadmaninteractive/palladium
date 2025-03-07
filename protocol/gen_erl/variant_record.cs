using Igor.Erlang;
using Igor.Erlang.Model;
using Igor.Erlang.AST;
using Igor;
using System.Collections.Generic;

public class VariantRecord : IErlangGenerator
{
    static string record = @"
-record({0}, {{
    {1}
}}).
";

    public static readonly BoolAttributeDescriptor VariantRecordEnabledAttribute = new BoolAttributeDescriptor("variant_record.enabled", IgorAttributeTargets.Any, AttributeInheritance.Scope);

    public void Generate(ErlModel model, Module module)
    {
        foreach (var form in module.Definitions)
        {
            if (form is VariantForm)
            {
                var variant = form as VariantForm;
                var enabled = form.attributes.Attribute(VariantRecordEnabledAttribute, false);
                if (enabled)
                {
                    var mod = model.Header(module.hrlFileName);
                    var recordName = variant.erlName;
                    var fields = new List<string>();
                    foreach (var f in variant.Fields)
                    {
                        if (!f.IsTag)
                            fields.Add(string.Format("{0} :: {1}", f.erlName, f.erlType));
                    }

                    //System.Console.WriteLine(string.Format(record, recordName, string.Join(",\n    ", fields)));
                    mod.Record(string.Format(record, recordName, string.Join(",\n    ", fields)));
                }
            }
        }
    }
}