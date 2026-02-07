------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              MIRRORING                                   --
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
--   ToDo: 


with ada.characters.handling;			use ada.characters.handling;


package body et_mirroring is
		

	function to_string (
		mirror	: in type_mirror)
		return string
	is
		s : string := to_lower (type_mirror'image (mirror));
	begin
		return s (mirror_prefix'length + 1 .. s'last);
	end to_string;


	
	function to_string (
		mirror	: in type_mirror;
		verbose : in boolean)
		return string 
	is begin
		if verbose then
			return "mirrored " & to_string (mirror);
		else
			return to_string (mirror);
		end if;
	end to_string;



	
	function to_mirror_style (
		style : in string) 
		return type_mirror 
	is begin
		return type_mirror'value (mirror_prefix & style);
	end to_mirror_style;




	procedure toggle_along_y (
		status : in out type_mirror)
	is begin
		case status is
			when MIRROR_NO =>
				status := MIRROR_ALONG_Y_AXIS;
				
			when MIRROR_ALONG_Y_AXIS =>
				status := MIRROR_NO;

			-- CS: This case should never happen:
			when MIRROR_ALONG_X_AXIS => 
				status := MIRROR_NO;
		end case;
	end;
		
		
		
		

	procedure toggle_along_x (
		status : in out type_mirror)
	is begin
		case status is
			when MIRROR_NO =>
				status := MIRROR_ALONG_X_AXIS;
				
			when MIRROR_ALONG_X_AXIS =>
				status := MIRROR_NO;

			-- CS: This case should never happen:
			when MIRROR_ALONG_Y_AXIS => 
				status := MIRROR_NO;
		end case;
	end;

	
	
end et_mirroring;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
