// Server-side utility functions

"use strict";

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
