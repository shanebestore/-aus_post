# cutting down dataset for script writing purposes

cut_1 <- subset(output_k_2, DESCRIPTION  %in% c("Express Courier International (eParcel)"))

cut_1 <-cut_1 [, c( "ARTICLE.ID", "weight_category_max",  "cubic_weight", "BILLED.WEIGHT"          
                                                  
                                                  
)]

"customer_code" , "NAME_1", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO." , "SERVICE.DATE" , "DESCRIPTION",
"BILLING.DATE", "CONSIGNMENT.ID", "article_id", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH",
"ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", "DECLARED.WEIGHT", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH", 
"DECLARED.WIDTH"	, "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH",	"DECLARED.WIDTH",
"DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "FROM.NAME", 	"FROM.ADDRESS",	"FROM.CITY",	"FROM.STATE",	"FROM.POSTAL.CODE",
"TO.NAME",	"TO.ADDRESS",	"TO.CITY",	"TO.STATE",	"TO.POSTAL.CODE", "CUST.REF.1",	"CUST.REF.2",	"BILLED.LENGTH", "BILLED.WIDTH",
"BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "CHARGE.CODE", "RECEIVING.COUNTRY", "CHARGE.ZONE.x", "Service", "QTY", "New_Column"