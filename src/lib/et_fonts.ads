------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              TEXT FONTS                                  --
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

with cairo;						use cairo;
with ada.strings.bounded; 		use ada.strings.bounded;


package et_fonts is
	

	font_family_length_max : constant positive := 50;
	
	package pac_font_family is new generic_bounded_length (font_family_length_max);

	
	function to_string (
		family : in pac_font_family.bounded_string) 
		return string;

	
	function to_family (
		family : in string) 
		return pac_font_family.bounded_string;


	type type_family is (
		FAMILY_MONOSPACE); -- others

	type type_slant is (
		SLANT_NORMAL,
		SLANT_ITALIC); -- CS others ?

	
	type type_weight is (
		WEIGHT_NORMAL); -- CS others ?
							
	
	type type_font is record
		family	: pac_font_family.bounded_string; -- string := "monospace";
		slant	: cairo_font_slant :=  CAIRO_FONT_SLANT_NORMAL;
		weight	: cairo_font_weight := CAIRO_FONT_WEIGHT_NORMAL;
	end record;


	-- This function buildss a font type
	-- from the given family, slant and weight:
	function to_font (
		family	: in type_family;
		slant	: in type_slant;
		weight	: in type_weight)
		return type_font;
	

end et_fonts;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
