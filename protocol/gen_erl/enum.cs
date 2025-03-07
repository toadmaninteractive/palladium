using Igor.Erlang;
using Igor.Erlang.Model;
using Igor.Erlang.AST;
using Igor;

public class EnumGenerator : IErlangGenerator
{
    static string func = @"
-spec {0}() -> {1}.

{0}() -> {0}.
";

    public enum EnumType
    {
        Fun,
        Macros,
    }

    public static readonly BoolAttributeDescriptor EnumEnabledAttribute = new BoolAttributeDescriptor("enum.enabled", IgorAttributeTargets.Enum, AttributeInheritance.Scope);
    public static readonly EnumAttributeDescriptor<EnumType> EnumTypeAttribute = new EnumAttributeDescriptor<EnumType>("enum.type", IgorAttributeTargets.Enum, AttributeInheritance.Scope);
    public static readonly StringAttributeDescriptor EnumNameAttribute = new StringAttributeDescriptor("enum.name", IgorAttributeTargets.Enum);

    public void Generate(ErlModel model, Module module)
    {
        foreach (var form in module.Enums)
        {
            var enabled = form.attributes.Attribute(EnumEnabledAttribute, false);
            if (!enabled)
                continue;

            var generatedType = form.attributes.Attribute(EnumTypeAttribute, EnumType.Macros);

            var name = form.attributes.Attribute(EnumNameAttribute, form.erlName);

            if (generatedType == EnumType.Fun)
            {
                var erl = model.Module(name);
                foreach (var value in form.Fields)
                {
                    erl.Export(value.erlName, 0);
                    erl.Function(string.Format(func, value.erlName, form.erlRemoteType));
                }
            }
            else if (generatedType == EnumType.Macros)
            {
                var erl = model.Header(name + ".hrl");
                //System.Console.WriteLine(erl.FileName + " " + module.hrlFileName);
                foreach (var value in form.Fields)
                {
                    erl.Define(name.ToUpper() + "_" + value.erlName.ToUpper(), value.erlName);
                }
            }
        }
    }
}