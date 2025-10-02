------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          PRIMITIVE OBJECTS                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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


with ada.text_io;				use ada.text_io;
-- with ada.strings;				use ada.strings;
-- with ada.strings.fixed;			use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;



package body et_primitive_objects is



	function to_shape (shape : in string) return type_shape is begin
		return type_shape'value (shape);
	end;

	
	function to_string (shape : in type_shape) return string is begin
		return to_lower (type_shape'image (shape));
	end;

	




	
	function to_string (filled : in type_filled) return string is begin
		return to_lower (type_filled'image (filled));
	end to_string;

	function to_filled (filled : in string) return type_filled is begin
		return type_filled'value (filled);
	end to_filled;


	
	-- FILL STYLE
	function to_string (fill_style : in type_fill_style) return string is begin
		return to_lower (type_fill_style'image (fill_style));
	end;

	function to_fill_style (fill_style : in string) return type_fill_style is begin
		return type_fill_style'value (fill_style);
	end;


	
end et_primitive_objects;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
