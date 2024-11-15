------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET IMPORT                                 --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.directories;

with et_project;
with et_string_processing;


package body et_import is

	procedure validate_cad_format (format : in string) is
		use et_string_processing;
	begin
		-- CS: use a loop to probe formats
		if format = to_lower (type_cad_format'image (kicad_v4)) then
			null;
		else
			log (ERROR, "CAD format '"
					& format & "' invalid !" 
					& " Supported formats: "
					& type_cad_format'image (kicad_v4) -- CS: use a loop to offer formats
					& " !",
				console => true);
			
			raise constraint_error;
		end if;
	end validate_cad_format;


	
	function to_cad_format (format : in string) return type_cad_format is
	begin
		return type_cad_format'value (format);
	end to_cad_format;


	
	function to_string (format : in type_cad_format) return string is
	-- Converts the given cad format to a string.
	begin
		return type_cad_format'image (format);
	end to_string;


	
	function invalid_cad_format (format : in type_cad_format) return string is
	-- Returns a message that the given format is not supported.
	begin
		return "CAD format '" & to_string (format) & "' not supported or invalid !";
	end invalid_cad_format;



	
	procedure validate_project (
		name		: in pac_project_name.bounded_string;
		cad_format	: in type_cad_format := UNKNOWN) 
	is
	-- CS: currently this is just a test, whether the directory "name" exists.
	-- CS: do a more detailled check depending on cad format (look for project files).
	begin
		if exists (pac_project_name.to_string (name)) then
			null; -- fine
		else
			log (ERROR, "project '" & to_string (name) 
				& "' not found ! Working directory correct ?",
				console => true);
			raise constraint_error;
		end if;
	end validate_project;
	
end et_import;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
