------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                KICAD                                     --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;

with et_general;				use et_general;
with et_kicad_general;			use et_kicad_general;
with kicad_coordinates;			use kicad_coordinates;
with et_string_processing;		use et_string_processing;
with et_kicad_libraries;		use et_kicad_libraries;
with et_kicad_packages;			use et_kicad_packages;

package et_kicad is

-- 	use et_kicad_libraries.pac_text;
	
	-- CS: a lot of stuff should move from here to et_kicad_general

	-- V4:
	pcb_new_version_4_0_7		: constant string (1..5)	:= "4.0.7";
	pcb_file_format_version_4	: constant string (1..1)	:= "4";
	
	host_name_pcbnew			: constant string (1..6)	:= "pcbnew";

	-- V5:
	pcb_new_version_5_0_0		: constant string (1..11)	:= "5.0.0-5.0.0"; -- CS update version number or find better solution
	-- Newly created projects without a board have a line like:
	--  (kicad_pcb (version 4) (host kicad "dummy file") )
	-- For this reason we need a constant:
	host_name_pcbnew_dummy_v5	: constant string (1..5)	:= "kicad";

	
	
	project_file_handle : ada.text_io.file_type;
	
    encoding_default 					: constant string (1..5) := "utf-8";	

	file_extension_project   			: constant string (1..3) := "pro";
	file_extension_schematic 			: constant string (1..3) := "sch";
	file_extension_schematic_lib		: constant string (1..3) := "lib";
	file_extension_board	 			: constant string (1..9) := "kicad_pcb";
    
	schematic_version_v4	: constant positive := 2; -- CS use dedicated type for schematic version
    schematic_version_v5	: constant positive := 4;

	procedure dummy;
	
end et_kicad;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
