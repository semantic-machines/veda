/**
 * Предикаты используемые в Veda Core
 */
module veda.core.common.know_predicates;

public const string owl__Ontology = "owl:Ontology";

public const string veda_schema__deleted             = "v-s:deleted";
public const string veda_schema__script              = "v-s:script";
public const string veda_schema__login               = "v-s:login";
public const string veda_schema__PermissionStatement = "v-s:PermissionStatement";
public const string veda_schema__PermissionFilter    = "v-s:PermissionFilter";
public const string veda_schema__useFilter           = "v-s:useFilter";
public const string veda_schema__created             = "v-s:created";
public const string veda_schema__permissionObject    = "v-s:permissionObject";
public const string veda_schema__permissionSubject   = "v-s:permissionSubject";
public const string veda_schema__AllResourcesGroup   = "v-s:AllResourcesGroup";
public const string veda_schema__Event               = "v-s:Event";
public const string veda_schema__resource            = "v-s:resource";
public const string veda_schema__memberOf            = "v-s:memberOf";
public const string veda_schema__Membership          = "v-s:Membership";
public const string veda_schema__owner               = "v-s:owner";
public const string veda_schema__fullUrl             = "v-s:fullUrl";

public const string rdf__type        = "rdf:type";
public const string rdfs__subClassOf = "rdfs:subClassOf";
public const string xsd__string      = "xsd:string";
public const string xsd__boolean     = "xsd:boolean";
public const string xsd__dateTime    = "xsd:dateTime";
public const string xsd__decimal     = "xsd:decimal";
public const string ticket__Ticket   = "ticket:Ticket";
public const string ticket__accessor = "ticket:accessor";
public const string ticket__when     = "ticket:when";
public const string ticket__duration = "ticket:duration";

public string[]     owl_tags = [ "rdf:Property", "owl:Restriction", "owl:ObjectProperty", "owl:DatatypeProperty", "owl:Class", "rdfs:Class" ];
