------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET IMPORT                               --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings;		 		use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.directories;			use ada.directories;
with ada.strings.bounded; 		use ada.strings.bounded;

with et_general;				use et_general;
with et_schematic;				use et_schematic;
with et_project;

with et_string_processing;		use et_string_processing;

package et_import is

	-- CAD FORMATS
	type type_cad_format is (
		UNKNOWN,
		KICAD_V4,
		KICAD_V5
		); -- CS: eagle_vx
	
	-- If no format specified via cmd line, a default applies so that the operator can be 
	-- notified about missing cad format.
	cad_format : type_cad_format := UNKNOWN; 

	procedure validate_cad_format (format : in string);

	function to_cad_format (format : in string) return type_cad_format;

	function to_string (format : in type_cad_format) return string;
	-- Converts the given cad format to a string.

	function invalid_cad_format (format : in type_cad_format) return string;
	-- Returns a message that the given format is not supported.
	
	procedure validate_project (
		name 		: in et_project.type_project_name.bounded_string;
		cad_format	: in type_cad_format := UNKNOWN);
	-- Checks if the given project of the given format exists in the current working directory.
	
end et_import;

-- Soli Deo Gloria
