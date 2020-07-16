------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            KICAD PROJECT                                 --
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
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.ordered_sets;
with ada.containers.indefinite_ordered_maps;
with ada.containers.vectors;

with et_general;				use et_general;
-- with et_project;
-- with et_geometry;
-- with et_schematic;
-- with et_terminals;
-- with et_packages;
-- with et_pcb;
with et_kicad_general;			use et_kicad_general;
with kicad_coordinates;			use kicad_coordinates;
-- with et_kicad_pcb;
-- with et_import;
-- with et_coordinates;			use et_coordinates;
-- use et_coordinates.pac_geometry_sch;

-- with et_pcb_coordinates;
with et_string_processing;		use et_string_processing;
-- with et_text;
-- with et_symbols;				--use et_symbols;
-- with et_devices;				use et_devices;
-- with et_frames;

with et_kicad;					use et_kicad;
-- with et_kicad_libraries;		use et_kicad_libraries;
-- with et_kicad_packages;			use et_kicad_packages;

package et_kicad_project is

	d : natural;
	
-- 	-- A collection of modules.
-- 	-- CS: Currently kicad does not support multiple modules (so called multi-board support).
-- 	-- Therefore the collection contains only one module.
-- 	package type_modules is new ordered_maps (
-- 		-- This is the module name like "MY_MOTOR_DRIVER" or "BLOOD_SAMPLE_ANALYZER"
-- 		key_type 		=> type_submodule_name.bounded_string,
-- 		"<" 			=> type_submodule_name."<",											 
-- 		element_type 	=> type_module);
-- 
-- 	modules : type_modules.map;
-- 	module_cursor : type_modules.cursor;


end et_kicad_project;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
