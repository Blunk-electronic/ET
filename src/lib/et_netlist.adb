------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET NETLIST DECLARATIONS                        --
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

with ada.containers;            use ada.containers;
with ada.containers.indefinite_ordered_maps;


with ada.directories;

with et_general;
with et_libraries;
with et_schematic;
with et_string_processing;		use et_string_processing;
with et_geometry;
with et_import;
with et_export;

package body et_netlist is

	procedure make_netlists is
	-- Netlists are to be exported in individual project directories in the work directory of ET.
	-- These project directories have the same name as the module indicated by module_cursor.
		use et_schematic;
		use et_schematic.type_rig;
		use et_export;
		use ada.directories;
		
		portlists : type_portlists.map;
	begin
		log (text => "building netlists ...", level => 1);

		-- We start with the first module of the rig.
		et_schematic.first_module;

		-- Process one rig module after another.
		-- module_cursor point to the module in the rig.
		while module_cursor /= type_rig.no_element loop
			log_indentation_up;
			log (text => "submodule " & to_string (key (module_cursor)), level => 1);
			create_project_directory (to_string (key (module_cursor)));

			rig_netlists.insert (
				key => key (module_cursor),
				new_item => type_netlist.empty_map);

			-- build_portlists generates the portlists of the rig module indicated
			-- by module_cursor.
			portlists := build_portlists;

			
			-- CS: write_netlist (build_portlists);
			
			next (module_cursor);
		end loop;

		log_indentation_down;
	end make_netlists;
	
end et_netlist;

-- Soli Deo Gloria
