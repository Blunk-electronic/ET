------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    ABSOLUTE AND RELATIVE COORDINATES                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;					use ada.text_io;

with ada.strings;					use ada.strings;
with ada.strings.fixed;				use ada.strings.fixed;
with ada.strings.unbounded;
with ada.characters.latin_1;
with ada.characters.handling;		use ada.characters.handling;



package body et_coordinates_abs_rel is
	



	function to_string (
		coordinates : in type_coordinates) 
		return string 
	is begin
		return space & to_lower (type_coordinates'image (coordinates));
	end;


	
	function to_coordinates (
		coordinates : in string)
		return type_coordinates 
	is begin
		return type_coordinates'value (coordinates);

-- 			exception
-- 				when event: others =>
-- 					log (text => ada.exceptions.exception_information (event), console => true);
-- 					raise;
	end;


	
	
end et_coordinates_abs_rel;

