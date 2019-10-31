------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             GENERAL_RW                                   --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			--use ada.characters
with ada.characters.latin_1;
with ada.containers;            use ada.containers;

with et_string_processing;

package general_rw is
	comment_mark : constant string := ("--");
									  
	function f (line : in et_string_processing.type_fields_of_line; position : in positive) return string;

	procedure expect_field_count (
		line			: in et_string_processing.type_fields_of_line;	-- the list of fields of the line
		count_expected	: in count_type;			-- the min. number of fields to expect
		warn			: in boolean := true); 		-- warn if too many fields

	procedure invalid_keyword (word : in string);

	section_begin	: constant string := "BEGIN]";	
	section_end		: constant string := "END]";

	section_drawing_grid	: constant string := "[DRAWING_GRID";
	
	keyword_name			: constant string := "name";
	keyword_appearance		: constant string := "appearance";
	keyword_description		: constant string := "description";
	keyword_meaning			: constant string := "meaning";
	keyword_class			: constant string := "class";
	keyword_value			: constant string := "value";
	keyword_partcode		: constant string := "partcode";	
	keyword_purpose			: constant string := "purpose";	
	keyword_width 			: constant string := "width";	
	
-- SECTIONS AND INDENTATION
	function write_top_level_reached return string;
	function write_enter_section return string;
	function write_return_to_section return string;
	function write_missing_begin_end return string;
	function write_section_stack_not_empty return string;
	
	procedure invalid_section;

	subtype type_tab_depth is natural range natural'first .. 9;
	tab_depth : type_tab_depth := type_tab_depth'first;
	
	tab : character renames et_string_processing.tabulator;

	procedure tab_depth_up;
	procedure tab_depth_down;
	procedure reset_tab_depth;

	type type_section_mark is (HEADER, FOOTER);	

	procedure section_mark (section : in string; mark : in type_section_mark);

	section_line	: constant string := "[LINE";
	section_arc		: constant string := "[ARC";
	section_circle	: constant string := "[CIRCLE";
	
	procedure line_begin;
	procedure line_end;			
	procedure arc_begin;
	procedure arc_end;
	procedure circle_begin;
	procedure circle_end;			

	section_text		: constant string := "[TEXT";
	section_texts		: constant string := "[TEXTS";
	
	section_placeholder		: constant string := "[PLACEHOLDER";
	section_placeholders	: constant string := "[PLACEHOLDERS";	

	section_package			: constant string := "[PACKAGE";	

	
	procedure text_begin;
	procedure text_end;
	procedure placeholder_begin;
	procedure placeholder_end;

	
	procedure write (
		keyword 	: in string;
		parameters	: in string;
		wrap		: in boolean := false);



-- GENERICS
	
	generic
		max : positive;
		type item is private;
	package stack_lifo is
		procedure push (x : in item);
		procedure pop;
		function pop return item;
		function depth return natural;
		procedure init;
		function empty return boolean;
		function current return item;
		function parent (degree : in natural := 1) return item;
		
	end stack_lifo;

	
end general_rw;
