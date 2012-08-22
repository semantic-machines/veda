<?php

/**
 * Keep in mind:
 * The code in this example is hard-coded, so previous/next and sorting will not work in the demo.
 * It was left out intentionally since this was designed to pull and order from another data source (like a database)
 *
 * The sample below is what you should build your code to look like when you incorporate your database or other storage system.
 * You'll also need to have the json_encode() function available to you. Otherwise you'll need another library like the Zend Framework.
 *
 **/

// Set json content-type header
header("Content-Type: text/x-json");

// Must let the datagrid know current PAGINATION info
// Page: The Current Page
// Pages: Total number of pages found
// Found: Total number of results found
// Displaying start: The First result we are _currently_ showing
// Displaying End: The last result we are _currently_ showing
$pager = array(array(
	'page'				=>	1,
	'pages'				=> 	1,
	'found' 			=>	5,
	'displayingStart'	=> 	1,
	'displayingEnd'		=> 	5
));

// Each array represents a column
// Each column must have:
	// ID	an unique identifier for that column
	// Display		the text to display in that column
	// Width		the width of that column (pixels or percentage)
	// Sort			optional. either sort-asc or sort-desc if you are currently sorting by that column
$headings = array(
	array(
		'id'			=>		'one',
		'display'		=>		'One',
		'width'			=>		'25%',
		'sort'			=>		''
	),
	array(
		'id'			=>		'two',
		'display'		=>		'Two',
		'width'			=>		'25%',
		'sort'			=>		'sort-asc'
	),
	array(
		'id'			=>		'three',
		'display'		=>		'Three',
		'width'			=>		'25%',
		'sort'			=>		''
	),
	array(
		'id'			=>		'four',
		'display'		=>		'Four',
		'width'			=>		'25%',
		'sort'			=>		''
	)
);

// Each array items represents a new row
// The array keys correspond to the id in the headings array above
$rows = array(
	array(
		'one' 	=> 'A1', 
		'two'	=>	'A2', 
		'three'	=>	'A3', 
		'four'	=> 	'A4',
	),
	array(
		'one' 	=> 'B1', 
		'two'	=>	'B2', 
		'three'	=>	'B3', 
		'four'	=> 	'B4',
	),
	array(
		'one' 	=> 'C1', 
		'two'	=>	'C2', 
		'three'	=>	'C3', 
		'four'	=> 	'C4',
	),
	array(
		'one' 	=> 'D1', 
		'two'	=>	'D2', 
		'three'	=>	'D3', 
		'four'	=> 	'D4',
	),
	array(
		'one' 	=> 'E1', 
		'two'	=>	'E2', 
		'three'	=>	'E3', 
		'four'	=> 	'E4',
	)
);

// Put it all in an array
$json = array(
	'pager' => $pager,
	'headings' => $headings,
	'rows' => $rows
);

// Encode it, then send it to the browser!
echo json_encode($json);

exit;

?>