-- ---------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM M-1 STRING PROCESSING                          --
--                                                                          --
--                                 M-1                                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:


-- with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.directories;			use ada.directories;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
-- with ada.float_text_io;			use ada.float_text_io;

with ada.containers;            use ada.containers;
--with ada.containers.vectors;
with ada.containers.indefinite_vectors;

-- with interfaces;				use interfaces;
-- with ada.exceptions;

with ada.calendar;				use ada.calendar;
with ada.calendar.formatting;	use ada.calendar.formatting;
with ada.calendar.time_zones;	use ada.calendar.time_zones;

package et_string_processing is

	function indentation ( width : in natural) return string;
	
	function date_now return string;

	short_string_length_max : constant natural := 10;
 	package type_short_string is new generic_bounded_length(short_string_length_max);

	universal_string_length_max	: constant natural := 1000;
	package type_universal_string is new generic_bounded_length(universal_string_length_max);
	
	long_string_length_max	: constant natural := 10000;
	package type_long_string is new generic_bounded_length(long_string_length_max);
	
	extended_string_length_max	: constant natural := 100000;
	package type_extended_string is new generic_bounded_length(extended_string_length_max);

	function ht_to_space (c : in character) return character;
	
	function wildcard_match (text_with_wildcards : in string; text_exact : in string) return boolean;
	-- Returns true if text_with_wildcards matches text_exact.
	-- text_with_wildcards is something like R41* , text_exact is something like R415
	
	function remove_comment_from_line(
		text_in : in string;					-- the input string
		comment_mark : in string;				-- the comment mark (like "--" or "#"
		test_whole_line : in boolean := true	-- when false, cares for the comment mark at line begin only
		)										-- further comment marks are ignored
		return string;

	function get_field_count (text_in : string) return natural;

	function strip_quotes (text_in : in string) return string;
	-- removes heading and trailing quotation from given string

	function enclose_in_quotes (text_in : in string; quote : in character := latin_1.apostrophe) return string;
	-- Adds heading and trailing quotate to given string. NOTE: apostrophe is ', quotation is "

	function trim_space_in_string (text_in : in string) return string;
	-- shrinks successive space characters to a single one in given string


	procedure write_message (
		file_handle : in ada.text_io.file_type;
		identation : in natural := 0;
		text : in string;
		lf   : in boolean := true;
		file : in boolean := true;
		console : in boolean := false);
	
	function get_field_from_line(
	-- Extracts a field separated by ifs at position. If trailer is true, the trailing content untiil trailer_to is also returned.
		text_in 	: in string;
		position 	: in positive;
		ifs 		: in character := latin_1.space;
		trailer 	: in boolean := false;
		trailer_to 	: in character := latin_1.semicolon
		) return string;

	-- CS: comments
	package type_list_of_strings is new indefinite_vectors (index_type => positive, element_type => string);

	-- This type is required when reading lines from files. It is a composite type
	-- whose components are hidden. The can only be accessed by special functions and procedures. See below.
	type type_fields_of_line is private;
	
	function read_line(
	-- Breaks down a given string and returns a type_fields_of_line.
		line			: in string; -- the line to be broken down
		number			: in positive_count; -- the line number	
		comment_mark	: in string; -- the comment mark like "--" or "#"
		test_whole_line	: in boolean := true; -- when false, cares for the comment mark at line begin only
												 -- further comment marks are ignored
		ifs				: in character := latin_1.space) -- field separator
		return type_fields_of_line;
	
	function append (left : in type_fields_of_line; right : in type_fields_of_line) return type_fields_of_line;
	
	function get_field_from_line (line : in type_fields_of_line; position : in positive) return string;

	function to_string ( line : in type_fields_of_line) return string;

	function affected_line ( line : in type_fields_of_line ) return string;
	-- Returns the line number of the given line in a string like "line x:"

	function field_count ( line : in type_fields_of_line) return count_type;
	-- Returns the number of fields in the given line.
	
	private
		type type_fields_of_line is record
			fields		: type_list_of_strings.vector;
			field_count	: count_type;
			number		: positive_count;
		end record;

	
end et_string_processing;

-- Soli Deo Gloria
