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

	function port_sits_on_segment (
	-- Returns true if the given port sits on the given net segment.
		port	: in type_port_base'class;
		segment	: in et_schematic.type_net_segment'class) 
		return boolean is

		use et_geometry;
		use et_coordinates;
		use et_string_processing;
		
		sits_on_segment : boolean := false;
		d : type_distance_point_from_line;

	begin
		-- First make sure port and segment share the same module path and sheet.
		-- It is sufficient to check against the segment start coordinates.
		if same_path_and_sheet (port.coordinates, segment.coordinates_start) then
			
			-- calculate the shortes distance of point from line.
			d := distance_of_point_from_line (
				point 		=> type_2d_point (port.coordinates),
				line_start	=> type_2d_point (segment.coordinates_start),
				line_end	=> type_2d_point (segment.coordinates_end),
				line_range	=> with_end_points);

			if (not d.out_of_range) and d.distance = et_coordinates.zero_distance then
				sits_on_segment := true;
				log ("port on segment", level => 5);
			end if;

		end if;
			
		return sits_on_segment;
	end port_sits_on_segment;

	function reference (port : in type_port) return string is
	-- Returns the component reference of the given port.	
	begin
		return et_schematic.to_string (port.reference);
	end reference;

	function port (port : in type_port) return string is
	-- Returns the port name of the given port.
	begin
		return et_libraries.to_string (port.port);
	end port;

	function pin (port : in type_port) return string is
	-- Returns the pin name of the given port.
	begin
		return et_libraries.to_string (port.pin);
	end pin;

	function appearance (port : in type_port) return et_libraries.type_component_appearance is
	-- Returns the appearance of the given port.
	begin
		return port.appearance;
	end appearance;
	
	function compare_ports (left, right : in type_port) return boolean is
	-- Returns true if left comes before right. Compares by component name and pin name.
	-- If left equals right, the return is false.	
	-- CS: needs verification !
		result : boolean := false;
		use et_libraries;
		use et_schematic;
	begin
		-- First we compare the component reference.
		-- Examples: C56 comes before R4, LED5 comes before LED7
		if compare_reference (left.reference, right.reference) then
			result := true;

		-- If equal references, compare pin names
		elsif type_pin_name.">" (left.pin, right.pin) then
			result := true;

		-- If equal pin names, compare port names -- CS: should never happen. raise alarm ?
		elsif type_port_name.">" (left.port, right.port) then
			result := true;
			
		else
			result := false;
		end if;

		-- in case of equivalence of left and right, we return false (default)
		return result;
	end compare_ports;

	function appearance (port : in type_ports.cursor) return et_libraries.type_component_appearance is
	-- Returns the appearance of the given port.
	begin
		return (element (port).appearance);
	end appearance;

	function build_portlists return type_portlists.map is
	-- Returns a list of components with the absolute positions of their ports as they are placed in the schematic.
		
	-- Locates the components of the schematic in the libraries. 
	-- Computes the absolute port positions of components from:
	--  - the port coordinates provided by the librares
	--  - the unit coordinates provided by the schematic
	--  - the unit mirror style provided by the schematic
	--  - the unit orientation provided by the schematic
	-- Special threatment for "common to all units" ports of global units. See comments.
	
	-- Stores the absolute port coordinates in map "portlists". 
	-- The key into this map is the component reference.

		-- Here we collect the portlists:
		portlists					: type_portlists.map;
		component_inserted			: boolean;
		component_cursor_portlists	: type_portlists.cursor; -- points to the portlist being built
	
		use et_libraries;
		use et_libraries.type_full_library_names;
		use et_schematic.type_components;
		use et_string_processing;

		-- This component cursor points to the schematic component being processed.
		component_cursor_sch: et_schematic.type_components.cursor;

		-- The generic name of a component in a library (like TRANSISTOR_PNP or LED) 
		-- is tempoarily held here:
		component_name		: et_libraries.type_component_name.bounded_string;

		-- The component reference in the schematic (like R44 or IC34)
		-- is tempoarily held here:
		component_reference	: et_libraries.type_component_reference;
	
		-- The library cursor points to the library to search in (in module.libraries).
		-- NOTE: module.libraries is just a list of full library names, no more.
		library_cursor_sch	: type_full_library_names.cursor;
		library_name		: type_full_library_name.bounded_string;

		-- This component cursor points to the library component being processed.
		use et_libraries.type_components;
		component_cursor_lib: et_libraries.type_components.cursor;

		-- CS: log_threshold for messages below

		-- For tempoarily storage of units of a component (taken from the schematic):
		units_sch : et_schematic.type_units.map;


		procedure extract_ports is
		-- Extracts the ports of the component indicated by component_cursor_lib.
		-- NOTE: The library contains the (x/y) positions of the ports.
			use et_libraries.type_units_internal;
			use et_libraries.type_ports;
			use et_coordinates;
			use et_schematic;
		
			-- The unit cursor of the component advances through the units stored in the library.
			unit_cursor_internal	: type_units_internal.cursor;

			-- The port cursor of the unit indicates the port of a unit.
			port_cursor				: et_libraries.type_ports.cursor; 

			unit_name_lib : type_unit_name.bounded_string; -- the unit name in the library. like "A", "B" or "PWR"
			unit_position : et_coordinates.type_coordinates; -- the coordinates of the current unit
			-- CS: external units

			procedure add_port is
			-- Builds a new port and appends it to portlist of the current 
			-- component (indicated by component_cursor_portlists).
			
			-- The library defined properties of the port are taken from where port_cursor points to.
			-- They are copied to the new port without change.
			
			-- Properites set in the schematic such as path, module name, sheet are copied into the
			-- new port unchanged. X and Y position of the port must be re-computed according to
			-- the rotation, mirror style and position of the unit in the schematic.
			-- NOTE: It is important first to rotate, then mirror (if required) and finally to move/offset it.

				procedure add (
					component	: in type_component_reference;
					ports		: in out type_base_ports.list) is
					use et_coordinates;
					
					port_coordinates : type_coordinates;

				begin -- add

					-- Init port coordinates with the coordinates of the port found in the library.
					-- The port position is a type_2d_point and must be converted to type_coordinates.
					et_coordinates.set (
						point		=> port_coordinates,
						position	=> to_coordinates (element (port_cursor).coordinates)); -- with type conversion

					-- rotate port coordinates
					rotate (
						point => port_coordinates,
						angle => et_schematic.orientation_of_unit (unit_name_lib, units_sch));

					-- Mirror port coordinates if required.
					case mirror_style_of_unit (unit_name_lib, units_sch) is
						when none => null; -- unit not mirrored in schematic
						when x_axis => mirror (point => port_coordinates, axis => x);
						when y_axis => mirror (point => port_coordinates, axis => y);
					end case;

					-- offset port coordinates by the coordinates of the unit found in the schematic
					move (point => port_coordinates, offset => unit_position);

					-- path remains unchanged because the port is still where the unit is
					set_path (port_coordinates, path (unit_position));

					-- module name remains unchanged because the port is still in the same module
					set_module (port_coordinates, module (unit_position));

					-- sheet name remains unchanged because the sheet is still the same
					set_sheet (port_coordinates, sheet (unit_position));
					
					-- insert a the newly built port in the portlist of the component
					type_base_ports.append (
						container => ports,
						new_item => (

							-- library defined properites:
							--port		=> key (port_cursor), -- the port name
							port		=> element (port_cursor).name, -- the port name
							pin			=> element (port_cursor).pin, -- the pin name
							direction	=> element (port_cursor).direction, -- the port direction
							style		=> element (port_cursor).style, -- port style

							-- We also set the port appearance. Later when writing the netlist, this property
							-- serves to tell real from virtual ports.
							appearance	=> et_schematic.component_appearance (component_cursor_sch),

							-- schematic defined properties:
							coordinates	=> port_coordinates
							));

					log_indentation_up;
					log (to_string (last_element (ports).direction), level => 2);
					-- CS: other port properties
					log (to_string (last_element (ports).coordinates), level => 2);
					log_indentation_down;
				end add;
				
			begin -- add_port
				-- We update the portlist of the component in container portlists.
				-- The cursor to the portlist was set when the element got inserted (see below in procedure build_portlists).
				type_portlists.update_element (
					container	=> portlists,
					position	=> component_cursor_portlists,
					process		=> add'access);
			end add_port;
			

			procedure ports_of_global_unit is
			-- Searches in the component (indicated by component_cursor_lib) for units
			-- with the "global" flag set.
			-- Sets the port_cursor for each port and leaves the rest of the work to procedure add_port.
				unit_cursor : type_units_internal.cursor;
			begin
				-- Loop in list of internal units:
				unit_cursor := first_internal_unit (component_cursor_lib);
				while unit_cursor /= type_units_internal.no_element loop
					log_indentation_up;

					if element (unit_cursor).global then
						--log ("global unit " & to_string (key (unit_cursor)));

						-- NOTE: One could think of exiting the loop here once the global unit
						-- has been found. If it were about KiCad only, this would make sense
						-- as there can be only one global unit per component.
						-- As for other CAE tools there might be more global units, so there
						-- is no early exit here.

						-- Loop in port list of the unit:						
						port_cursor := first_port (unit_cursor); -- port in library
						while port_cursor /= et_libraries.type_ports.no_element loop

							--log ("port " & type_port_name.to_string (key (port_cursor))
							log ("port " & type_port_name.to_string (element (port_cursor).name)
								& " pin/pad " & to_string (element (port_cursor).pin));

							-- Build a new port and append port to portlist of the 
							-- current component (indicated by component_cursor_portlists).
							add_port;
							
							port_cursor := next (port_cursor);
						end loop;
					end if;

					log_indentation_down;
					unit_cursor := next (unit_cursor);
				end loop;

			end ports_of_global_unit;

			
		begin -- extract_ports
			-- Loop in unit list of the component (indicated by component_cursor_lib).
			-- unit_cursor_internal points to the unit in the library.
			-- Frequently, not all units of a component are deployed in the schematic.
			-- If a unit is not deployed it is ignored. Otherwise the coordinates of the
			-- unit in the schematic are stored in unit_position.

			-- Init the unit cursor of the current component:
			unit_cursor_internal := first_internal_unit (component_cursor_lib);

			-- Loop in list of internal units:
			while unit_cursor_internal /= type_units_internal.no_element loop
				log_indentation_up;

				-- get the unit name
				unit_name_lib := key (unit_cursor_internal);

				-- Now the unit name serves as key into the unit list we got from the schematic (unit_sch).
				-- If the unit is deployed in the schematic, we load unit_position. 
				-- unit_position holds the position of the unit in the schematic.
				if unit_exists (name => unit_name_lib, units => units_sch) then -- if unit deployed in schematic
					log ("unit " & to_string (unit_name_lib));
					unit_position := position_of_unit (name => unit_name_lib, units => units_sch); -- pos. in schematic
					log_indentation_up;
					log (to_string (unit_position));

					-- Get the ports of the current unit. Start with the first port of the unit.
					-- The unit_position plus the relative port position (in library) yields the absolute
					-- position of the port (in schematic).

					-- Init port cursor
					port_cursor := first_port (unit_cursor_internal); -- port in library

					-- Loop in port list of the unit:
					while port_cursor /= et_libraries.type_ports.no_element loop
						log_indentation_up;
						--log ("port " & type_port_name.to_string (key (port_cursor))
						log ("port " & type_port_name.to_string (element (port_cursor).name)
							& " pin/pad " & to_string (element (port_cursor).pin));
						
						-- Build a new port and append port to portlist of the 
						-- current component (indicated by component_cursor_portlists).
						add_port;
						
						log_indentation_down;
						port_cursor := next (port_cursor);
					end loop;

					-- SEARCH FOR PORTS OF GLOBAL UNITS. 
					
					-- NOTE: Take a rest before trying to understand the following:
					
					-- The problem with ports that are "common to all units" (KiCad terminology) is:
					--  The unit they belong to does not appear in the schematic, whereas their ports
					--  are visible on each unit (kicad button "show hidden pins").
					-- Solution: We assume all "common to all units" ports belong to all units of the 
					-- component, thus inheriting the unit_name_lib and the unti_position.
					-- The the unit_name_lib and unit_position of the current unit are applied
					-- to the global units.
					ports_of_global_unit;
					
					log_indentation_down;
				end if;

				log_indentation_down;
				unit_cursor_internal := next (unit_cursor_internal);
			end loop;
			
		end extract_ports;
	

		procedure check_appearance_sch_vs_lib is
		-- Verifies appearance of schematic component against library component.
		begin
			if et_schematic.component_appearance (component_cursor_sch) = 
			   et_libraries.component_appearance (component_cursor_lib) then
				null; -- fine
			else
				-- this should never happen
				log (text => message_error & "comonent appearance mismatch !", console => true);
				-- CS: provide more details on the affected component
				raise constraint_error;
			end if;
		end check_appearance_sch_vs_lib;
					

	begin -- build_portlists
		log_indentation_up;
		log (text => "generating portlists ...");
		log_indentation_up;

		-- The library contains the coordinates of the ports whereas
		-- the schematic provides the coordinates of the units of a component.
		-- These coordinates summed up yields the absolute position of the ports.
		
		-- Loop in component list of schematic. component_cursor_sch points to the 
		-- particular component. 

		-- We ignore components that are "power_flags". Since the portlists are a prerequisite
		-- of netlist generation, this implies, that "power_flags" are not in the netlists.
		-- Yet other virtual components like
		-- power symbols like GND or P3V3 are relevant indeed. Because later when we do
		-- the netlist post-processing they enforce their port names to the connected net.
		
		-- For each component, store a list of its units in units_sch.
		-- This list contains the units found in the schematic with their coordinates.
		-- These coordinates plus the port coordinates (extracted in 
		-- procedure (extract_ports) will later yield the absolute positions of the ports.
		et_schematic.reset_component_cursor (component_cursor_sch);
		while component_cursor_sch /= et_schematic.type_components.no_element loop

			-- power flags are to be skipped
			if not et_schematic.component_power_flag (component_cursor_sch) then
		
				-- log component by its reference		
				component_reference :=  et_schematic.component_reference (component_cursor_sch);
				log (text => "reference " & et_schematic.to_string (component_reference));
				
				-- Insert component in portlists. for the moment the portlist of this component is empty.
				-- After that the component_cursor_portlists points to the component. This cursor will
				-- later be used to add a port to the portlists.
				type_portlists.insert (
					container	=> portlists,
					key			=> component_reference, -- like R44
					new_item	=> type_base_ports.empty_list,
					inserted	=> component_inserted, -- obligatory, no further meaning
					position	=> component_cursor_portlists -- points to the portlist being built
					);
				
				-- get the units of the current schematic component (indicated by component_cursor_sch)
				units_sch := et_schematic.units_of_component (component_cursor_sch);
	
				-- get generic component name (as listed in a library)
				log_indentation_up;			
				component_name := et_schematic.component_name_in_library (component_cursor_sch);
				log (text => "generic name " & to_string (component_name));

				-- Search in libraries for a component with this very generic name.
				-- library_cursor_sch points to the particular full library name.
				-- The libraries are searched according to their order in the library list of the module.
				-- The search is complete on the first finding of the component.
				log_indentation_up;
				log (text => "searching in libraries ...");
				log_indentation_up;
				et_schematic.reset_library_cursor (library_cursor_sch);
				while library_cursor_sch /= type_full_library_names.no_element loop

					-- Set and log particular library to be searched in.
					library_name := (element (library_cursor_sch));
					log (text => to_string (library_name));

					-- Get cursor of that component in library. If cursor is empty, search in
					-- next library. If cursor points to a matching component, extract ports
					-- of that component. Procedure extract_ports uses component_cursor_lib .
					component_cursor_lib := find_component (library_name, component_name);
					if component_cursor_lib = et_libraries.type_components.no_element then
						-- not found -> advance to next library (in module.libraries)
						next (library_cursor_sch); 
					else
						-- As a safety measure we make sure that the appearance of the component
						-- in the schematic equals that in the library.
						check_appearance_sch_vs_lib;
	
						extract_ports; -- uses component_cursor_lib
						-- found -> no further search required
						-- CS: write warning if component exists in other libraries ?
						exit;
					end if;
						
				end loop;

				-- IF COMPONENT NOT FOUND IN ANY LIBRARY:
				-- Early exits from the loop above leave library_cursor_sch pointing to a library.
				-- If the loop has been completed without success, library_cursor_sch points to no_element.
				-- If all libraries searched without any match -> generate error message.
				if library_cursor_sch = type_full_library_names.no_element then
					log_indentation_reset;
					log (message_error & "component with reference "  
						& et_schematic.to_string (et_schematic.component_reference (component_cursor_sch))
						& " has no generic model in any library !",
						console => true);
					raise constraint_error;
				end if;
				
				log_indentation_down;
				log_indentation_down;
				log_indentation_down;

			end if; -- no power_flag
			
			next (component_cursor_sch); -- advance to next component
		end loop;

		log_indentation_down;
		log_indentation_down;
		
		return portlists;
	end build_portlists;

	function first_port (component_cursor : in type_portlists.cursor) return type_base_ports.cursor is
	-- Returns a cursor pointing to the first port of a component in the portlists.
		port_cursor : type_base_ports.cursor;
	
		procedure set_cursor (
			name : in et_libraries.type_component_reference;
			ports : in type_base_ports.list) is
		begin
			port_cursor := first (ports);
		end set_cursor;
		begin -- first_port
		type_portlists.query_element (
			position => component_cursor,
			process => set_cursor'access);

		return port_cursor;
	end first_port;

	
	
	procedure first_module is
	-- Resets the module_cursor to the first module of the rig.
	begin
		module_cursor := rig.first;
		-- CS: exception handler in case given module does not exist
	end first_module;

	function first_net return type_netlist.cursor is
	-- Returns a cursor to the first net of the current module (indicated by module_cursor).
		cursor : type_netlist.cursor;
	
		procedure set (
			module	: in et_coordinates.type_submodule_name.bounded_string;
			netlist	: in type_netlist.map) is
		begin
			cursor := netlist.first;
		end set;

	begin -- first_net
		type_rig_netlists.query_element (
			position => module_cursor,
			process => set'access);

		return cursor;
	end first_net;

	function net_count return count_type is
	-- Returns the number of nets of the current module as string.
		count : count_type := 0;
	
		procedure get (
			module	: in et_coordinates.type_submodule_name.bounded_string;
			netlist	: in type_netlist.map) is
		begin
			count := length (netlist);
		end get;

	begin
		type_rig_netlists.query_element (
			position => module_cursor,
			process => get'access);

		return count;
	end net_count;
	
	function first_port (net_cursor : in type_netlist.cursor) return type_ports.cursor is
	-- Returns a cursor to the first port of the given net in the current module (indicated by module_cursor).
		cursor : type_ports.cursor;
	
		procedure set (
			net		: in et_schematic.type_net_name.bounded_string;
			ports	: in type_ports.set) is
		begin
			cursor := ports.first;
		end set;

	begin -- first_port
		type_netlist.query_element (
			position => net_cursor,
			process => set'access);

		return cursor;
	end first_port;

	function port_count (net_cursor : in type_netlist.cursor) return count_type is
	-- Returns the number of ports of the given net of the current module.
		count : count_type := 0;

		procedure get (
			net		: in et_schematic.type_net_name.bounded_string;
			ports	: in type_ports.set) is
		begin
			count := length (ports);
		end get;
					  
	begin -- port_count
		type_netlist.query_element (
			position => net_cursor,
			process => get'access);
		return count;
	end port_count;

	function component_ports_total return count_type is
	-- Returns the total number of component ports in the current module.
	-- The return does not include so called "power_flags".
		n : count_type := 0;

		procedure get (
			module	: in et_coordinates.type_submodule_name.bounded_string;
			netlist	: in type_netlist.map) is

			net : type_netlist.cursor;
		begin
			net := netlist.first;

			-- loop trough the nets and summ up the number of ports.
			while net /= type_netlist.no_element loop
				n := n + port_count (net);
				next (net);
			end loop;
		end get;

	begin -- component_ports_total
		type_rig_netlists.query_element (
			position => module_cursor,
			process => get'access);

		return n;

	end component_ports_total;
	
	procedure set_module (module_name : in et_coordinates.type_submodule_name.bounded_string) is
	-- Sets the active module. Leaves module_cursor pointing to the module.
	begin
		module_cursor := rig.find (module_name);
	end set_module;
	
	
	procedure make_netlists is
	-- Builids the netlists of all modules in the rig.
	-- Bases on the portlists and net-segment information of the module.
	-- Netlists become exported in individual project directories in the work directory of ET.
	-- These project directories have the same name as the module indicated by module_cursor.

		use et_schematic;
		use et_schematic.type_rig;
		use et_export;
		use ada.directories;

		-- Here the portlists of the current module are stored by function build_portlists:
		portlists : type_portlists.map;

		function make_netlist return type_netlist.map is
		-- Generates the netlist of the current module (indicated by module_cursor).
		-- The portlists provide the port coordinates. 
		-- The module provides the nets.
		-- With this information we make the netlist of the current module.
		
			net_cursor_netlist : type_netlist.cursor; -- points to the current net within the netlist being built
			net_inserted : boolean; -- indicates whether a net has been inserted into the netlist
			
			net_cursor_module : type_nets.cursor := first_net; 
			-- points to the net being read from the module (see type_module in et_schematic.ads)
		
			netlist_pre : type_netlist.map; -- the preliminary netlist
			use type_nets;
			
			segment_cursor : type_net_segments.cursor; -- points to the net segment being read from the module
			segment : type_net_segment; -- the actual net segment read from the module
			use type_net_segments;

			component_cursor : type_portlists.cursor; -- point to the component within the portlists
			use type_portlists;

			port_cursor : type_base_ports.cursor; -- points to the port being read from a component within the portlists
-- 			port : type_port_base; -- the actual port read from the portlists
			use type_base_ports;

			use et_string_processing;

			procedure add_port (
			-- Adds the given port and the component reference to the net in the netlist being built.
				reference : in et_libraries.type_component_reference;
				port : in type_port_base) is

				procedure add (
					net_name : in type_net_name.bounded_string;
					ports : in out type_ports.set) is

					inserted : boolean;
					cursor : type_ports.cursor;
				begin
					--ports.insert (port with reference); -- CS: does not compile with gnat 4.8.5 . may work with later versions
		
					-- If a port sits on the point where two segments meet, the same port should be inserted only once.
					-- Thus we have the obligatory flag "inserted". 
					ports.insert (
						new_item => (port with reference),
						inserted => inserted,
						position => cursor
						);

					if not inserted then -- port already in net
						log_indentation_up;
						log (text => "already processed -> skipped", level => 3);
						log_indentation_down;
					end if;
						
				end add;
				
			begin -- add_port
				netlist_pre.update_element (
					position => net_cursor_netlist,
					process => add'access);
			end add_port;

			function port_to_net_name (port_name : in et_libraries.type_port_name.bounded_string) 
			-- Converts the given port name to a net name.
				return type_net_name.bounded_string is
			begin
				-- cs: count renamings. warning if more than one renaming occured
				return type_net_name.to_bounded_string (et_libraries.to_string (port_name));
			end port_to_net_name;
																				  
			function post_process_netlist return type_netlist.map is
			-- Post processes the netlist of the current rig module (indicated by module_cursor)
			-- Enforces port names onto nets. A net name must change according to the names of 
			-- "power out" ports in that net. Example: A net with name N$2 has a port with name "GND"
			-- the net name must change from N$2 to GND.
				net_cursor_pre	: type_netlist.cursor;

				netlist_post 	: type_netlist.map; -- the netlist being built. to be returned finally.
				net_cursor_post	: type_netlist.cursor;
				inserted_post	: boolean;
				net_name		: et_schematic.type_net_name.bounded_string;

				port_cursor		: type_ports.cursor;
				port			: type_port;

				procedure append_ports (
					net_name	: in et_schematic.type_net_name.bounded_string;
					ports		: in out type_ports.set) is
				begin
					union (target => ports, source => element (net_cursor_pre)); 
				end append_ports;
				
				use et_libraries;
			begin -- post_process_netlist
			
				log (text => "post-processing module netlist ...", level => 1);
				-- LOOP IN PRELIMINARY NETLIST
				net_cursor_pre := netlist_pre.first;
				while net_cursor_pre /= type_netlist.no_element loop
					net_name := key (net_cursor_pre);

					log_indentation_up;
					log (text => "net " & et_schematic.type_net_name.to_string (net_name), level => 1);
					log_indentation_up;

					-- LOOP IN PORTLIST OF CURRENT NET
		-- 			--log (text => "exporting" & count_type'image (port_count (net_cursor)) & " ports ...", level => 1);
					port_cursor := first_port (net_cursor_pre);
					while port_cursor /= type_ports.no_element loop
						port := element (port_cursor);
						
						log (text => 
								reference (port) & " "
								& et_netlist.port (port) & " "
								& et_netlist.pin (port) & " "
								);

						-- Test if supply port name matches net name. When positive, nothing to to.
						-- Otherwise set the new net name according to the port name of the power port.
						-- The last renaming that took place owerwrites previous renamings.
						if port.direction = POWER_OUT then
							if et_netlist.port (element (port_cursor)) = et_schematic.to_string (net_name) then
								null; -- fine. net name matches port name
							else
								log (text => "port name overrides net name", level => 1);

								-- Check if a non-anonymous net name is overridded by the port name.
								-- Example: The net name is already "P3V3" and the port name is "+3V3".
								-- More serious example : The net name is already "P3V3" and the port name is "GND".
								if not et_schematic.anonymous (net_name) then
									log (message_warning & "NAME OF POWER OUT PIN OVERRIDES NET NAME !");
									-- CS: error and abort instead ?
								end if;
									
								net_name := port_to_net_name (element (port_cursor).port);
								-- CS: update net names in module.nets ?
							end if;
						end if;
						
						next (port_cursor);
					end loop;

					-- create a new net in netlist_post
					netlist_post.insert (
						key => net_name,
						new_item => element (net_cursor_pre),
						position => net_cursor_post,
						inserted => inserted_post);

					-- if net already in netlist_post, append ports only
					if not inserted_post then
						netlist_post.update_element (
							position => net_cursor_post,
							process => append_ports'access);
					end if;

					log_indentation_down;
					log_indentation_down;

					next (net_cursor_pre);
				end loop;
				
				return netlist_post;
			end post_process_netlist;
					
		begin -- make_netlist (note singluar !)
			log (text => "building module netlist ...", level => 1);

			-- HOW DOES IT WORK ?
			-- The outer loop fetches one net after another from the module (see type_module in et_schematic.ads).
			-- The net name is used as primary key for the net being inserted into the netlist.
			-- The loop in the next level fetches the net segments from the net.

			-- The loop in the next level fetches components from the portlists.
			-- The loop in the next level fetches the ports of the component within the portlists.

			-- Then we test if a port sits on the current segment. When positive, the port is added to 
			-- the net within the netlist.
			
			-- LOOP IN NETS OF MODULE (outer loop)
			while net_cursor_module /= type_nets.no_element loop

				-- get the net name via net_cursor
				log_indentation_up;
				--log (text => "net " & et_schematic.type_net_name.to_string (key (net_cursor_module)), level => 2);
				log (text => "net " & et_schematic.to_string (element (net_cursor_module).name), level => 2);
				
				-- Insert net in netlist with the net name as primary key:
				netlist_pre.insert (
					--key => key (net_cursor_module), -- net name like "MCU_CLOCK"
					key => element (net_cursor_module).name, -- net name like "MCU_CLOCK"
					new_item => type_ports.empty_set, -- for the moment an empty portlist
					inserted => net_inserted, -- CS: check status ? In case a strand with same name was already read.
					position => net_cursor_netlist); 
					-- NOTE: if net not inserted due to a previous strand with the same name, net_cursor_netlist still
					-- points to the net in netlist_pre where ports are to be added.

				-- loop in segments of net
				segment_cursor := first_segment (net_cursor_module);
				while segment_cursor /= type_net_segments.no_element loop
					segment := element (segment_cursor);
					log_indentation_up;
					log (text => "probing segment " & to_string (segment), level => 2);

					-- LOOP IN PORTLISTS
					component_cursor := first (portlists);
					while component_cursor /= type_portlists.no_element loop
						log_indentation_up;
						log ("probing component " & et_schematic.to_string (key (component_cursor)), level => 3);
						
						port_cursor := first_port (component_cursor);
						while port_cursor /= type_base_ports.no_element loop

							-- CS: skip already processed ports to improve performance
							
-- 							port := element (port_cursor); -- in portlist of component
							log_indentation_up;
							--log ("probing port " & et_coordinates.to_string (port.coordinates), level => 3);
							log ("probing port " & et_coordinates.to_string (element (port_cursor).coordinates), level => 3);

							-- test if port sits on segment
-- 							if port_sits_on_segment (port, segment) then
							if port_sits_on_segment (element (port_cursor), segment) then
								log_indentation_up;
								log ("comp " & et_schematic.to_string (key (component_cursor))
									& " port " 
									& et_libraries.to_string (element (port_cursor).port)
									& " "
									& et_coordinates.to_string (element (port_cursor).coordinates), level => 2);
								
								log_indentation_down;

								-- add port to current net in netlist
								add_port (
									reference => key (component_cursor),
									--port => port);
									port => element (port_cursor));

							end if;
							log_indentation_down;
							
							next (port_cursor);
						end loop;

						log_indentation_down;
						next (component_cursor);
					end loop;

					log_indentation_down;
					next (segment_cursor);
				end loop;

				log_indentation_down;
				next (net_cursor_module);
			end loop;

			log (text => "processed nets" & count_type'image (length (netlist_pre)), level => 2);

			return post_process_netlist;

		end make_netlist;
		
	begin -- make_netlists (note plural !)
		log (text => "building rig netlists ...", level => 1);

		-- We start with the first module of the rig.
		et_schematic.first_module;

		-- Process one rig module after another.
		-- module_cursor points to the module in the rig.
		while et_schematic.module_cursor /= type_rig.no_element loop
			log_indentation_up;
			log (text => "module " & to_string (key (et_schematic.module_cursor)), level => 1);
			create_project_directory (to_string (key (et_schematic.module_cursor)));

			-- Generate the portlists of the module indicated by module_cursor.
			portlists := build_portlists;

			-- Insert the module in rig_netlists with the netlist built by make_netlist:
			rig.insert (
				key => key (et_schematic.module_cursor),
				new_item => make_netlist);

			next (et_schematic.module_cursor);
		end loop;

		log_indentation_down;
	end make_netlists;


	procedure write_netlists is
	-- Exports/Writes the netlists of the rig in separate files.
	-- Call this procedure after executing procedure make_netlist !
		use type_rig_netlists;
		use ada.directories;
		use et_general;
		use et_libraries;
		
		netlist_handle : ada.text_io.file_type;
		netlist_file_name : type_netlist_file_name.bounded_string;
	
		net_cursor	: type_netlist.cursor; -- points to the net being exported
		net_name	: et_schematic.type_net_name.bounded_string;
		port_cursor	: type_ports.cursor; -- point to the port being exported
		port		: type_port;

	begin -- write_netlists
		first_module;
		
		log (text => "writing rig netlists ...", level => 1);
		while module_cursor /= type_rig_netlists.no_element loop
			log_indentation_up;
			log (text => "module " & to_string (key (module_cursor)), level => 1);

			-- compose the netlist file name and its path like "../ET/motor_driver/CAM/motor_driver.net"
			netlist_file_name := type_netlist_file_name.to_bounded_string 
				(
				compose (
					containing_directory => compose 
						(
						containing_directory => compose (work_directory, to_string (key (module_cursor))),
						name => et_export.directory_cam
						),
					name => to_string (key (module_cursor)),
					extension => extension_netlist)
				);

			-- create the netlist (which inevitably and intentionally overwrites the previous file)
			log_indentation_up;
			log (text => "creating netlist file " & type_netlist_file_name.to_string (netlist_file_name), level => 1);
			create (
				file => netlist_handle,
				mode => out_file, 
				name => type_netlist_file_name.to_string (netlist_file_name));

			put_line (netlist_handle, comment_mark & " " & system_name & " netlist");
			put_line (netlist_handle, comment_mark & " date " & string (date_now));
			put_line (netlist_handle, comment_mark & " module " & to_string (key (module_cursor)));
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			-- CS: statistics about net count and pin count ?
			put_line (netlist_handle, comment_mark & " legend:");
			put_line (netlist_handle, comment_mark & "  net name");
			put_line (netlist_handle, comment_mark & "  component port pin/pad direction");
			put_line (netlist_handle, comment_mark & " " & row_separator_single);
			
			-- export net
			log_indentation_up;
			log (text => "exporting" & count_type'image (net_count) & " nets ...", level => 1);
			net_cursor := first_net;
			while net_cursor /= type_netlist.no_element loop
				net_name := key (net_cursor);

				-- write net name in netlist
				log_indentation_up;
				log (text => "net " & et_schematic.type_net_name.to_string (net_name), level => 1);

				-- Check if net has no dedicated name (a net name like N$3 is anonymous):
				if et_schematic.anonymous (net_name) then
					log (message_warning & "Net has no dedicated name !");
				end if;
				
				new_line (netlist_handle);
				put_line (netlist_handle, et_schematic.type_net_name.to_string (net_name));

				-- export port
				log_indentation_up;
				log (text => "exporting" & count_type'image (port_count (net_cursor)) & " ports ...", level => 1);
				-- CS: warning if net has only one pin

				-- CS: perfom an ERC on the current net in a separate loop (similar to the one below)
				
				port_cursor := first_port (net_cursor);
				while port_cursor /= type_ports.no_element loop

					-- we export only ports of real components
					if appearance (port_cursor) = et_libraries.sch_pcb then

						port := element (port_cursor);

						-- write reference, port, pin in netlist (all in a single line)
						-- CS: use port_cursor instead of a variable "port"
						put_line (netlist_handle, 
							reference (port) & " "
							& et_netlist.port (port) & " "
							& et_netlist.pin (port) & " "
							);

					end if;
						
					next (port_cursor);
				end loop;
				log_indentation_down;
				
				log_indentation_down;

				next (net_cursor);
			end loop;

			new_line (netlist_handle);
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			put_line (netlist_handle, comment_mark & " end of list");
			
			close (netlist_handle);

			log_indentation_down;
			log_indentation_down;
			log_indentation_down;

			
			next (module_cursor);
		end loop;
		
	end write_netlists;
		
end et_netlist;

-- Soli Deo Gloria
