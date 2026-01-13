------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        GENERAL READ AND WRITE                            --
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
with ada.containers;            use ada.containers;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;


package et_general_rw is

	section_begin	: constant string := "BEGIN]";	
	section_end		: constant string := "END]";


	
	section_drawing_grid	: constant string := "[DRAWING_GRID";
	keyword_name			: constant string := "name";
	keyword_appearance		: constant string := "appearance";
	
	
-- SECTIONS AND INDENTATION
	function write_top_level_reached return string;
	function write_enter_section return string;
	function write_return_to_section return string;
	function write_missing_begin_end return string;
	function write_section_stack_not_empty return string;
	
	procedure invalid_section;


	section_line	: constant string := "[LINE";
	section_arc		: constant string := "[ARC";
	section_circle	: constant string := "[CIRCLE";

	section_text		: constant string := "[TEXT";
	section_texts		: constant string := "[TEXTS";
	
	section_placeholder		: constant string := "[PLACEHOLDER";
	section_placeholders	: constant string := "[PLACEHOLDERS";	

	section_package			: constant string := "[PACKAGE";	

	

	-- Outputs a message telling that start and end point of are have differing 
	-- distance to center.
	procedure invalid_arc;

	

	
end et_general_rw;
