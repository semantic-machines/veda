function get_property_chain (ticket, uri, element) 
{ 
	var doc = get_individual (ticket, uri); 
	var doc_first = doc;
	var field;

	for (var i = 1; i < arguments.length; i++) 
	{
//		if (arguments[i] in doc)
//		{
		field = doc[arguments[i]];
		if (field)		
		{
			doc = get_individual (ticket, field[0].data); 	
			if (!doc)
			    break;
		}
//		}
			
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

function toJson (x)
{
    return JSON.stringify (x, null, 2); 
}

function newDocument (type, fields)
{
    
}

function getUri (field)
{
    if (field && field.length > 0)
    {
	    return field[0].data;
    }    
}

function guid() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
               .toString(16)
               .substring(1);
  }
  return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
         s4() + '-' + s4() + s4() + s4();
}