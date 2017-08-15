------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET SCHEMATIC DECLARATIONS                        --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

with et_general;
with et_string_processing;
with et_import;

package body et_schematic is

	function to_string (position : in type_coordinates) return string is
	-- Returns the given position as string.
	begin
		return ("position (x/y/sheet) " & 
			trim(et_general.type_grid'image(position.x),left) & "/" &
			trim(et_general.type_grid'image(position.y),left) & "/" &
			trim( positive'image(position.sheet_number),left));
		-- CS: exception handler
	end to_string;

	
	function to_string (orientation : in type_orientation) return string is
	-- Returns the the given orientation as string.
	begin
		return("orientation " & et_general.type_orientation'image(orientation));
		-- CS: exception handler
	end to_string;
	

end et_schematic;
-- Soli Deo Gloria
