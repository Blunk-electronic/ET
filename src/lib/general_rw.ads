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


	function f (line : in et_string_processing.type_fields_of_line; position : in positive) return string;

	procedure expect_field_count (
		line			: in et_string_processing.type_fields_of_line;	-- the list of fields of the line
		count_expected	: in count_type;			-- the min. number of fields to expect
		warn			: in boolean := true); 		-- warn if too many fields

	procedure invalid_keyword (word : in string);

	section_begin	: constant string := "BEGIN]";	
	section_end		: constant string := "END]";
	

-- INDENTATION
	subtype type_tab_depth is natural range natural'first .. 9;
	tab_depth : type_tab_depth := type_tab_depth'first;
	
	tab : character renames et_string_processing.tabulator;
	space : character renames ada.characters.latin_1.space;

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
	section_placeholder	: constant string := "[PLACEHOLDER";
	
	procedure text_begin;
	procedure text_end;
	procedure placeholder_begin;
	procedure placeholder_end;

	
	procedure write (
		keyword 	: in string;
		parameters	: in string;
		space 		: in boolean := false;
		wrap		: in boolean := false);

	
end general_rw;
