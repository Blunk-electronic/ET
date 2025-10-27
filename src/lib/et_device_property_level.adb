------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICE PROPERTY LEVEL                          --
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
-- <http://www.gnu.org/licenses/>.   
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

with ada.text_io;					use ada.text_io;
with ada.characters.latin_1;
with ada.characters.handling;



package body et_device_property_level is


	

	function to_string (
		level	: in type_properties_level)
		return string
	is begin
		return type_properties_level'image (level);
	end;


	
	
	function to_properties_level (
		level	: in string;
		error	: out boolean)
		return type_properties_level
	is 
		use ada.characters.handling;
		s : string := to_upper (level);
	begin
		error := false;
		
		if s = "L1" then
			return DEVICE_PROPERTIES_LEVEL_1;
		elsif s = "L2" then
			return DEVICE_PROPERTIES_LEVEL_2;
		elsif s = "L3" then
			return DEVICE_PROPERTIES_LEVEL_3;
		else
			error := true;
			return DEVICE_PROPERTIES_LEVEL_1;
		end if;
	end;
	
	
	
	
end et_device_property_level;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
