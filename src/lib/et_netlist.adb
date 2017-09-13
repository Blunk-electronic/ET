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


	procedure dummy is begin null; end dummy;

	function build_portlists return type_portlists.map is
	-- Locates the components of the schematic in the libraries. 
	-- Extracts the ports of the components from the libraries and
	-- stores them in map "portlists". The key into this map is the 
	-- component reference.
		
		portlists : type_portlists.map;

		use et_libraries;
		use et_libraries.type_full_library_names;
		--use et_schematic;
		use et_schematic.type_components;

		-- This component cursor points to the schematic component being processed.
		component_cursor_sch: et_schematic.type_components.cursor;

		-- The generic name of a component in a library (like TRANSISTOR_PNP or LED) 
		-- is tempoarily held here:
		component_name		: et_libraries.type_component_name.bounded_string; 

		-- The library cursor points to the library to search in (in module.libraries):
		library_cursor_sch	: type_full_library_names.cursor;

		library_name		: type_full_library_name.bounded_string;

		-- This component cursor points to the library component being processed.
		use et_libraries.type_components;
		component_cursor_lib: et_libraries.type_components.cursor;

		-- CS: log_threshold for messages below

		procedure extract_ports is

			use et_libraries.type_units_internal;
			
			unit_cursor_internal	: type_units_internal.cursor;
			unit_count_internal		: count_type;
			units_internal			: type_units_internal.map;
			-- CS: external units
		begin
					-- number of internal units
			unit_count_internal := length (element (component_cursor_lib).units_internal);
			log ("number of internal units" & count_type'image (unit_count_internal));					
			units_internal := element (component_cursor_lib).units_internal;

			case unit_count_internal is

				when 0 => 
					-- component has no units 
					raise constraint_error; -- CS: this should never happen
					
				when others =>

					unit_cursor_internal := first (units_internal);
					
					loop exit when unit_cursor_internal = type_units_internal.no_element;
						log ("unit " & type_unit_name.to_string (key (unit_cursor_internal)));

						unit_cursor_internal := next (unit_cursor_internal);
					end loop;
				
			end case;
		end extract_ports;
	
	begin
		
		log (text => "component list");
		log_indentation_up;
		
		et_schematic.reset_component_cursor (component_cursor_sch);

		-- Loop in component list of schematic:
		while component_cursor_sch /= et_schematic.type_components.no_element loop

			-- log component by its reference
			log (text => "reference " & et_schematic.to_string (et_schematic.component_reference (component_cursor_sch)));

			-- load component name as it is listed in a library
			log_indentation_up;			
			component_name := et_schematic.component_name_in_library (component_cursor_sch);
			log (text => "generic name " & to_string (component_name));

			-- Search in libraries for a component with name component_name.
			-- The libraries are searched according to their order in the library list of the module.
			-- The search is complete on the first finding of the component.
			log_indentation_up;
			log (text => "searching libraries ...");
			log_indentation_up;
			et_schematic.reset_library_cursor (library_cursor_sch);
			while library_cursor_sch /= type_full_library_names.no_element loop

				-- set and log library name
				library_name := (element (library_cursor_sch));
				log (text => to_string (library_name));

				-- Search component in library
				component_cursor_lib := find_component (library_name, component_name);

				-- If component not found, advance to next library and search again.
				-- If component found, exit loop and proceed with next component.
				if component_cursor_lib = type_components.no_element then
					-- not found -> advance to next library (in module.libraries)
					next (library_cursor_sch); 
				else
					extract_ports;
					-- found -> no further search required
					-- CS: write warning if component exists in other libraries ?
					exit;
				end if;
				
			end loop;

			-- If component not found in any library:
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
