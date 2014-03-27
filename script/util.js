function get_property_chain (uri, element) 
{ 
	var doc = get_individual (uri); 
	var doc_first = doc;
	var field;

	for (var i = 1; i < arguments.length; i++) 
	{
		field = doc[arguments[i]];
		if (field)		
		{
			doc = get_individual (field[0].data); 	
			if (!doc)
			    break;
		}
			
	}		

	var res = {};

	if (field != undefined)
	{
	    res.field = field;
	    res.first = doc_first;
	    res.last = doc;
	}
	return res;	
}
