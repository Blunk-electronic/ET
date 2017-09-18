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


package body et_netlist is


	function build_portlists return type_portlists.map is
	-- Returns a list of components with the absolute positions of their ports.
		
	-- Locates the components of the schematic in the libraries. 
	-- Extracts the ports of the components from the libraries and
	-- stores them in map "portlists". The key into this map is the 
	-- component reference.

		-- Here we collect the portlists of schematic components.
		portlists					: type_portlists.map;
		component_inserted			: boolean;
		component_cursor_portlists	: type_portlists.cursor;
	
		use et_libraries;
		use et_libraries.type_full_library_names;
		use et_schematic.type_components;

		-- This component cursor points to the schematic component being processed.
		component_cursor_sch: et_schematic.type_components.cursor;

		-- The generic name of a component in a library (like TRANSISTOR_PNP or LED) 
		-- is tempoarily held here:
		component_name		: et_libraries.type_component_name.bounded_string; 
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

		function unit_exists (
		-- Returns true if the given unit with name exists in the given list of units.
			name : in type_unit_name.bounded_string; -- the unit being inquired
			units : in et_schematic.type_units.map) -- the list of units
			return boolean is

			use et_schematic;
			use et_schematic.type_units;
		begin
			if et_schematic.type_units.find (container => units, key => name) = type_units.no_element then
				return false;
			else	
				return true;
			end if;
		end unit_exists;
	
		function position_of_unit (
		-- Returns the coordinates of the given unit.
		-- The unit is one element in the given list of units.
			name : in type_unit_name.bounded_string; -- the unit being inquired
			units : in et_schematic.type_units.map) -- the list of units
			return et_schematic.type_coordinates is
			unit_cursor : et_schematic.type_units.cursor;
		begin
			unit_cursor := et_schematic.type_units.find (container => units, key => name);
			return et_schematic.type_units.element (unit_cursor).position;
		end position_of_unit;

	
		procedure extract_ports is
		-- Extracts the ports of the component as indicated by the current component_cursor_lib.
		-- The unit cursor of the component advances through the units stored in the library.
		-- NOTE: The library contains the positions of the ports.
			use et_libraries.type_units_internal;
			use et_libraries.type_ports;
			
			unit_cursor_internal	: type_units_internal.cursor; -- points to the current unit
			port_cursor				: et_libraries.type_ports.cursor; -- points to a port of that unit

			unit_name_lib : type_unit_name.bounded_string; -- the unit name in the library. like "A", "B" or "PWR"
			unit_position : et_schematic.type_coordinates; -- the coordinates of the current unit
			-- CS: external units

			procedure add_port is
			-- Builds a new port and appends it to portlist of the current 
			-- component (indicated by component_cursor_portlists).
			
			-- The library defined properties of the port are taken from where port_cursor points to.
			-- They are copied to the new port without change.
			
			-- Properites set in the schematic such as path, module name, sheet are copied into the
			-- new port unchanged. X and Y position in turn become offset by the X/Y position of the 
			-- unit.
			
				procedure add (
					component	: in type_component_reference;
					ports		: in out type_ports.list) is
				begin
					type_ports.append (
						container => ports,
						new_item => (

							-- library defined properites:
							port		=> key (port_cursor), -- the port name
							pin			=> element (port_cursor).pin, -- the pin name
							direction	=> element (port_cursor).direction, -- the port direction
							style		=> element (port_cursor).style, -- port style

							-- schematic defined properties:
							coordinates	=> (
								path			=> unit_position.path, -- path remains unchanged
								module_name		=> unit_position.module_name, -- module name unchanged
								sheet_number	=> unit_position.sheet_number, -- sheet unchanged

								-- CS: offset port position by unit position, orientation an mirroring
								x				=> unit_position.x + element (port_cursor).coordinates.x, 
								y				=> unit_position.y + element (port_cursor).coordinates.y 
							)));

					log_indentation_up;
					log (et_schematic.to_string (last_element (ports).coordinates));
					log_indentation_down;
				end add;
				
			begin -- add_port
				type_portlists.update_element (
					container => portlists,
					position => component_cursor_portlists,
					process => add'access);
			end add_port;
			
		begin -- extract_ports
			-- Loop in unit list of the component (indicated by component_cursor_lib).
			-- unit_cursor_internal points to the unit in the library.
			-- Get the coordinates of the same unit in the schematic.
			unit_cursor_internal := first_internal_unit (component_cursor_lib);
			while unit_cursor_internal /= type_units_internal.no_element loop
				log_indentation_up;

				-- get the unit name
				unit_name_lib := key (unit_cursor_internal);

				-- Now the unit name serves as key into the unit list we got from the schematic (unit_sch).
				-- So we get the unit position in the schematic.
				if unit_exists (unit_name_lib, units_sch) then
					log ("unit " & to_string (unit_name_lib));
					unit_position := position_of_unit (unit_name_lib, units_sch);
					log_indentation_up;
					log (et_schematic.to_string (unit_position));

					-- Get the ports of the current unit. Start with the first port of a unit.
					-- The unit_position plus the relative port position yields the absolute
					-- position of the port.
					port_cursor := first_port (unit_cursor_internal);
					while port_cursor /= et_libraries.type_ports.no_element loop
						log_indentation_up;
						log ("port " & type_port_name.to_string (key (port_cursor)));
						
						-- Build a new port and append port to portlist of the 
						-- current component (indicated by component_cursor_portlists).
						add_port;
						
						log_indentation_down;
						port_cursor := next (port_cursor);
					end loop;

					log_indentation_down;
				end if;
					
				log_indentation_down;
				unit_cursor_internal := next (unit_cursor_internal);
			end loop;
			
		end extract_ports;
	
	begin -- build_portlists
		
		log (text => "generating portlists ...");
		log_indentation_up;

		-- The library contains the coordinates of the ports whereas
		-- the schematic provides the coordinates of the units of a component.
		-- These coordinates summed up yields the absolute position of the ports.
		
		-- Loop in component list of schematic. component_cursor_sch points to the 
		-- particular component. For each component, store a list
		-- of its units in units_sch. This list contains the units found in the schematic
		-- with their coordinates. These coordinates plus the port coordinates (extracted in 
		-- procedure (extract_ports) will yield the absolute positions of the ports.
		et_schematic.reset_component_cursor (component_cursor_sch);
		while component_cursor_sch /= et_schematic.type_components.no_element loop

			-- log component by its reference		
			component_reference :=  et_schematic.component_reference (component_cursor_sch);
			log (text => "reference " & et_schematic.to_string (component_reference));
			
			-- Insert component in portlists. for the moment the portlist of this component is empty.
			-- After that the component_cursor_portlists points to the component. This cursor will
			-- later be used to add a port to the portlists.
			type_portlists.insert (
				container	=> portlists,
				key			=> component_reference,
				new_item	=> type_ports.empty_list,
				inserted	=> component_inserted, -- obligatory, no further meaning
				position	=> component_cursor_portlists
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
			log (text => "searching libraries ...");
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
				if component_cursor_lib = type_components.no_element then
					-- not found -> advance to next library (in module.libraries)
					next (library_cursor_sch); 
				else
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
			
			next (component_cursor_sch); -- advance to next component
		end loop;

		log_indentation_down;
		
		return portlists;
	end build_portlists;
	
end et_netlist;
-- Soli Deo Gloria
