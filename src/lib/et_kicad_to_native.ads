------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            KICAD TO NATIVE                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
with ada.strings.maps;				use ada.strings.maps;
with ada.strings.bounded;   	    use ada.strings.bounded;
with ada.containers;        	    use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_nets;						use et_nets;
with et_project;
with et_general;
with et_coordinates;
with et_string_processing;
with et_pcb;
with et_pcb_coordinates;

package et_kicad_to_native is

	use pac_net_name;
	
	procedure transpose (log_threshold : in et_string_processing.type_log_level);
	-- Transposes coordinates of schematic and layout elements:
	-- 1. In schematic changes the path (selector of et_coordinates.type_coordinates) to the root path (/).
	-- 2. Moves schematic objects from negative to positive y coordinates.
	--    (The origin in kicad is the upper left corner. The origin in ET is the lower left corner.)
	
	procedure to_native (
		project_name	: in et_project.pac_project_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level);
	-- Converts the kicad module (incl. component libraries) to a native module.
	-- Converts the packages (from package_libraries) to native packages.
	-- NOTE: Packages of the board (incl. their deviations/modifications
	-- from the package_libraries) are ignored !
	-- Creates a directory named after project_name and saves the module in that directory.
		
end et_kicad_to_native;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
