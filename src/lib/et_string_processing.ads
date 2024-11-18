-- ---------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           STRING PROCESSING                              --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:


with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.containers;            use ada.containers;
with ada.containers.indefinite_vectors;


package et_string_processing is


	row_separator_length 	: constant positive := 100;
	row_separator_single	: constant string (1..row_separator_length)	:= row_separator_length * "-";	
	row_separator_double	: constant string (1..row_separator_length)	:= row_separator_length * "=";
-- 	item_not_specified		: constant string (1..7) := "missing";

	
-- WARNING AND ERROR MESSAGES
	function metric_system return string;
	-- Returns a message about the metric system used.

	
	-- Returns a message about the degrees used.
	function angles_in_degrees return string;
	

	-- Removes a possible trailing directory separtor.
	-- Removes the trailing directory separtor (if preset).
	function strip_directory_separator (text : in string) return string;

	
	function ht_to_space (c : in character) return character;

	
	-- Replaces a tilde by space. Other characters are returned unchanged.
	function tilde_to_space (c : in character) return character;


	
	-- Returns true if text_with_wildcards matches text_exact.
	-- text_with_wildcards is something like R41* , text_exact is something like R415
	function wildcard_match (
		text_with_wildcards	: in string; 
		text_exact 			: in string) 
		return boolean;


	
	function remove_comment_from_line (
		text_in 		: in string;			-- the input string
		comment_mark	: in string;			-- the comment mark (like "--" or "#"
		test_whole_line	: in boolean := true)	-- when false, cares for the comment mark at line begin only
		return string;							-- further comment marks are ignored
		

	
	-- Field numbers:
	subtype type_field_count is natural 
		range 0 .. 20; -- CS increase if required

	
	-- Field positions start with 1:
	subtype type_field_count_positive is type_field_count 
		range 1 .. type_field_count'last;

	
	
	-- Returns the number of fields in a given string.
	-- Example: For the given string "This is a dummy text"
	-- the field count is 5:
	function get_field_count (
		text_in : in string) 
		return type_field_count;


	
	-- removes heading and trailing quotation from given string
	function strip_quotes (
		text_in : in string) 
		return string;


	
	-- Adds heading and trailing quotate to given string. 
	-- NOTE: apostrophe is ', quotation is "
	function enclose_in_quotes ( -- CS rename to quote
		text_in	: in string;
		quote	: in character := latin_1.apostrophe) 
		return string;


	
	-- Adds heading and trailing quotate to given character.
	-- NOTE: apostrophe is ', quotation is "
	function enclose_in_quotes ( -- CS rename to quote
		charcter_in	: in character;
		quote		: in character := latin_1.apostrophe) 
		return string;


	-- Reduces successive space characters 
	-- to a single one:
	function trim_spaces (
		text_in : in string) 
		return string;

	
	-- Removes a trailing directory separator.
	-- Handles both Windows and Linux separators (\ and /):
	function remove_trailing_directory_separator (
		path_in : in string) 
		return string;
	

	
	-- Returns true if given string is a number. 
	-- CS: See body for things to do:
	function is_number (
		text : in string) 
		return boolean;



	
	
	-- Extracts a field separated by ifs at position. If trailer is true, the 
	-- trailing content until trailer_to is also returned.
	function get_field_from_line (
		text_in 	: in string;
		position 	: in type_field_count_positive;
		ifs 		: in character := latin_1.space;
		trailer 	: in boolean := false;
		trailer_to 	: in character := latin_1.semicolon)
		return string;

	
	
	-- This type serves to collect strings. It MUST be a vector, because
	-- this allows do pick out arbitrary strings by their indexes:
	package pac_list_of_strings is new indefinite_vectors (
		index_type		=> positive, 
		element_type	=> string);


	
	-- This type is required when reading lines from files. 
	-- It is a composite type whose components are hidden. 
	-- They can only be accessed by special functions 
	-- and procedures. See below.
	type type_fields_of_line is private;




	
	-- Breaks down a given string and returns a type_fields_of_line.
	function read_line (
		-- The line to be processed and broken down in fields:
		line			: in string; 
						   
		-- The line number				   
		number			: in positive := positive'first;	

		-- The comment mark like "--" or "#":
		comment_mark	: in string;

		-- When false, cares for the comment mark at line begin only
		-- further comment marks are ignored:
		test_whole_line	: in boolean := true;

		-- The field separator:
		ifs				: in character := latin_1.space;

		-- True if text in delimiters is to be wrapped into a single field:
		delimiter_wrap	: in boolean := false;

		-- The text delimiter sign (mostly "):
		delimiter		: in character := latin_1.quotation)
		return type_fields_of_line;




	
	
	-- Appends a field to a line:
	procedure append_field (
		line	: in out type_fields_of_line;
		field	: in string);


	
	-- Append right fields to left fields:
	function append_field (
		left	: in type_fields_of_line;
		right	: in type_fields_of_line) 
		return type_fields_of_line;


	
	-- Remove fields from line:
	function remove_field (
		line	: in type_fields_of_line;
		first	: in type_field_count_positive;
		last	: in type_field_count_positive)
		return type_fields_of_line;



	-- Sets the content of a given field in a line:
	procedure set_field (
		line		: in out type_fields_of_line;
		position	: in type_field_count_positive;
		content		: in string);


	
	-- Returns the field at the given position. Raises constraint error if there is no 
	-- field at given position.
	function get_field (
		line		: in type_fields_of_line;
		position	: in type_field_count_positive) 
		return string;


	
	function to_string (line : in type_fields_of_line) return string;

	
	-- Returns the line number of the given line.
	function get_line_number (
		line : in type_fields_of_line) 
		return positive;

	
	-- Returns the line number of the given line in a string like "line x:"
	function get_affected_line (
		line : in type_fields_of_line) 
		return string;

	
	-- Returns the number of fields in the given line.
	function get_field_count (
		line : in type_fields_of_line) 
		return type_field_count;

	
	function lines_equally (left, right : in type_fields_of_line) return boolean;

	
private
	
	
	type type_fields_of_line is record
		fields		: pac_list_of_strings.vector;
		field_count	: type_field_count := type_field_count'first; -- number of fields in line
		number		: positive := positive'first; -- line numer
	end record;


		
end et_string_processing;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
