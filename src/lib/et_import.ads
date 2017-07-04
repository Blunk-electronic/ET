------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET IMPORT                               --
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
-- 
-- with ada.strings.bounded; 		use ada.strings.bounded;
-- with ada.containers; 			use ada.containers;
-- with ada.containers.vectors;
-- with ada.containers.doubly_linked_lists;

with et_schematic;				use et_schematic;

package et_import is

    procedure dummy;

	-- FILE AND DIRECTORY NAMES
	directory_report		: constant string (1..10) := "et_reports";
	file_extension_report	: constant string (1..3) := "txt";	
	file_report_import 		: constant string (1..6) := "import";

    
	-- CAD FORMATS
	type type_cad_format is ( kicad_v4 );
    

end et_import;

