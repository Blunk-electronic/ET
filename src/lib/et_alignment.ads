------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              ALIGNMENT                                   --
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
--

with ada.strings;				use ada.strings;
with ada.containers; 			use ada.containers;

with et_string_processing;		use et_string_processing;


package et_alignment is
	
	
	alignment_prefix	: constant string := ("ALIGN_");
	
	-- The alignment refers to the anchor point of the text.
	-- The anchor point is usually where the origin of the text is.
	type type_text_alignment_horizontal is (ALIGN_LEFT, ALIGN_CENTER, ALIGN_RIGHT);
	
	function to_string (alignment : in type_text_alignment_horizontal) return string;
	function to_alignment_horizontal (alignment : in string) return type_text_alignment_horizontal;

	
	
	type type_text_alignment_vertical is (ALIGN_TOP, ALIGN_CENTER, ALIGN_BOTTOM);
	
	function to_string (alignment : in type_text_alignment_vertical) return string;
	function to_alignment_vertical (alignment : in string) return type_text_alignment_vertical;

	
	type type_text_alignment is record
		horizontal	: type_text_alignment_horizontal := ALIGN_LEFT;
		vertical	: type_text_alignment_vertical := ALIGN_BOTTOM;
	end record;

	text_alignment_default : constant type_text_alignment := (ALIGN_LEFT, ALIGN_BOTTOM);

	
	function to_alignment (
		line : in type_fields_of_line; -- "alignment horizontal center vertical center"
		from : in type_field_count_positive)
		return type_text_alignment;

	
	function to_string (alignment : in type_text_alignment) return string;


end et_alignment;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
