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
with et_coordinates;
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
	
		function make_netlist return type_netlist.map is
			net_cursor : type_nets.cursor := first_net;
			netlist : type_netlist.map;
			use type_nets;
			
			segment_cursor : type_net_segments.cursor;
			segment : type_net_segment;
			use type_net_segments;

			component_cursor : type_portlists.cursor;
			use type_portlists;

			port_cursor : type_base_ports.cursor;
			port : type_port_base;
			use type_base_ports;

			use et_string_processing;
		begin
			-- LOOP IN NETS OF MODULE
			while net_cursor /= type_nets.no_element loop

				-- get the net name via net_cursor
				log (text => "net " & et_schematic.type_net_name.to_string (key (net_cursor)));
				
				-- insert net in netlist
				netlist.insert (
					key => key (net_cursor), -- net name like "MCU_CLOCK"
					new_item => type_ports.empty_set); -- for the moment an empty portlist

				segment_cursor := first_segment (net_cursor);
				while segment_cursor /= type_net_segments.no_element loop
					segment := element (segment_cursor);

					-- LOOP IN PORTLISTS
					component_cursor := first (portlists);
					while component_cursor /= type_portlists.no_element loop
						--log (et_coordinates.to_string (port.coordinates));
						
						port_cursor := first_port (component_cursor);
						while port_cursor /= type_base_ports.no_element loop
							port := element (port_cursor);

							-- test if port sits on segment
-- 							if port_sits_on_segment (port, segment) then
								log_indentation_up;
								log (et_coordinates.to_string (port.coordinates));
								log_indentation_down;

-- 							end if;
							
							next (port_cursor);
						end loop;
						
						next (component_cursor);
					end loop;
					
					next (segment_cursor);
				end loop;
				
				next (net_cursor);
			end loop;
			
			-- CS: write_netlist (build_portlists);
			return type_netlist.empty_map;
		end make_netlist;
		
	begin
		log (text => "building netlists ...", level => 1);

		-- We start with the first module of the rig.
		et_schematic.first_module;

		-- Process one rig module after another.
		-- module_cursor point to the module in the rig.
		while module_cursor /= type_rig.no_element loop
			log_indentation_up;
			log (text => "module " & to_string (key (module_cursor)), level => 1);
			create_project_directory (to_string (key (module_cursor)));

			-- Generate the portlists of the module indicated by module_cursor.
			portlists := build_portlists;
			
			rig_netlists.insert (
				key => key (module_cursor),
				new_item => make_netlist);

			next (module_cursor);
		end loop;

		log_indentation_down;
	end make_netlists;
	
end et_netlist;

-- Soli Deo Gloria
