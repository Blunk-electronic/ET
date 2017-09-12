------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET EXPORT                               --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
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

with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings;		 		use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.directories;			use ada.directories;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
with ada.containers.ordered_maps;
-- with ada.containers.doubly_linked_lists;

with et_general;
with et_schematic;
with et_libraries;

package et_export is

	
	file_report_export	: constant string (1 .. 
								et_general.report_directory'length + 1 -- containing directory + separator
								+ 6 -- base name
								+ 1 + et_general.report_extension'length) -- separator + extension
									:= compose (et_general.report_directory, "export", et_general.report_extension);
	report_handle		: ada.text_io.file_type;

	
	-- CAD FORMATS
	type type_cad_format is ( et_v1, kicad_v4 );
	-- If no format specified via cmd line, a default applies so that the operator can be 
	-- notified about missing cad format.
	cad_format : type_cad_format := type_cad_format'first; 

	procedure create_report;
	-- Creates the report file in report_directory.
	-- Sets the output to the report file.
	-- Leaves the report file open for further puts.

	procedure close_report;
	-- Writes the report footer and closes the report file.
	-- Sets the output back to standard_output.


	
end et_export;

-- Soli Deo Gloria
