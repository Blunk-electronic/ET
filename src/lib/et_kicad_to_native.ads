------------------------------------------------------------------------------
--                                                                          --
--                       SYSTEM ET KICAD_TO_NATIVE                          --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 
--		1. Objects like net segments, net labels, junctions, notes ... 
--		   should be collected in ordered sets instead of doubly_linked_lists
--			- the benefits: placing identical objects at the same position would be impossible
--			- the cons: ordering subprograms required
--		2. Templates via copy and via reference
--		3. Assembly variants

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_coordinates;
with et_libraries;
with et_string_processing;
with et_pcb;
with et_pcb_coordinates;

package et_kicad_to_native is

	procedure transpose (log_threshold : in et_string_processing.type_log_level);
	-- Transposes coordinates of schematic and layout elements:
	-- 1. In schematic changes the path (selector of et_coordinates.type_coordinates) to the root path (/).
	-- 2. Moves schematic objects from negative to positive y coordinates.
	--    (The origin in kicad is the upper left corner. The origin in ET is the lower left corner.)
	
	procedure to_native (log_threshold : in et_string_processing.type_log_level);
	-- Converts the kicad module (incl. component libraries) to a native module.
	-- Converts the packages (from package_libraries) to native packages.
	-- NOTE: Packages of the board (incl. their deviations/modifications
	-- from the package_libraries) are ignored !
	-- Saves the module in project_path (see below) in a module file (*.mod).
		
end et_kicad_to_native;

-- Soli Deo Gloria
