------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET SCHEMATIC DECLARATIONS                      --
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

with ada.directories;

with ada.containers;            use ada.containers;
-- with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;

with et_coordinates;
with et_general;
with et_libraries;
with et_string_processing;
with et_geometry;
with et_import;


package body et_schematic is

	function to_string (schematic : in type_schematic_file_name.bounded_string) return string is
	-- Returns the given schematic file name as string.
	begin
		return type_schematic_file_name.to_string (schematic);
	end to_string;
	
	-- Sometimes we need to output the location of a submodule:
	procedure write_path_to_submodule is
		c : type_path_to_submodule.cursor;
		use et_string_processing;
	begin
		log (text => "path/location:");
		log_indentation_up;
		
		c := type_path_to_submodule.first (path_to_submodule);            

		-- If there is a hierarchy deeper than 1, write path to submodule:
		if type_path_to_submodule.length(path_to_submodule) > 1 then
			for n in 1..type_path_to_submodule.length(path_to_submodule)-1 loop
				log (text => type_submodule_name.to_string(type_path_to_submodule.element(c)));
				c := type_path_to_submodule.next(c);
			end loop;
		
			c := type_path_to_submodule.last(path_to_submodule);

			-- write the submodule name
			log_indentation_up;
			log (text => type_submodule_name.to_string(type_path_to_submodule.element(c)));
			log_indentation_down;
		else
			-- no hierarchy. write just the submodule name
			log (text => type_submodule_name.to_string(type_path_to_submodule.element(c)));
		end if;
		
		log_indentation_down;
	end write_path_to_submodule;

	-- Here we append a submodule name the the path_to_submodule.
	procedure append_name_of_parent_module_to_path (submodule : in type_submodule_name.bounded_string) is
		use et_string_processing;
		use ada.directories;
	begin
		log (text => "path_to_submodule: appending submodule " & type_submodule_name.to_string(submodule), level => 1);
		-- Since we are dealing with file names, the extension must be removed before appending.
		type_path_to_submodule.append (path_to_submodule,
			type_submodule_name.to_bounded_string (base_name (type_submodule_name.to_string(submodule)))
			);
	end append_name_of_parent_module_to_path;
	
	-- Here we remove the last submodule name form the path_to_submodule.
	procedure delete_last_module_name_from_path is
	begin
		type_path_to_submodule.delete_last (path_to_submodule);
	end delete_last_module_name_from_path;


	
	function to_string (position : in type_coordinates) return string is
	-- Returns the given position as string.
	begin
		return coordinates_preamble
			& trim (positive'image (position.sheet_number),left) 
			& et_libraries.coordinates_dimension_separator
			& trim (et_libraries.type_grid'image(position.x),left)
			& et_libraries.coordinates_dimension_separator
			& trim (et_libraries.type_grid'image(position.y),left);

		-- CS: output in both mil and mm
		
		-- CS: exception handler
	end to_string;

	function to_string (net_name : in type_net_name.bounded_string) return string is
	-- Returns the given net name as string.
	begin
		return type_net_name.to_string (net_name);
	end to_string;
	
	procedure write_label_properties (label : in type_net_label) is
	-- Writes the properties of the given net label in the logfile.
		use et_string_processing;
		use et_libraries;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		case label.label_appearance is
			when simple =>
				log (text => "simple label");
			when tag =>
				log (text => "tag label");
				-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_up;
		log ("name '" & type_net_name.to_string (label.text) & "' ");
		log (to_string (et_libraries.type_coordinates (label.coordinates)), log_threshold);
		log (et_libraries.to_string (label.orientation), log_threshold);
		
		case label.label_appearance is
			when simple =>
				null;
			when tag =>
				null;
				--put("tag label ");
				-- CS: directon, global, hierarchic, style, ...
		end case;

		log_indentation_down;
		log_indentation_down;

	end write_label_properties;

	procedure write_note_properties (note : in et_schematic.type_note) is
	-- Writes the properties of the given note
		use et_string_processing;
		use et_libraries;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		log ("text note");

		log_indentation_up;

		-- content
		if et_libraries.type_text_content.length (note.content) > 0 then
			log ("content '" & type_text_content.to_string (note.content) & "'");
		else
			log (et_string_processing.message_warning & "no content !"); 
		end if;

		
		if log_level >= log_threshold then
			
			-- position
			log (to_string (et_libraries.type_coordinates (note.coordinates)), log_threshold);
			
			-- size
			log ("size" & et_libraries.type_text_size'image (note.size));

			-- style
			log ("style " & to_lower(et_libraries.type_text_style'image (note.style)));

			-- line width
			log ("line width" & et_libraries.type_text_line_width'image (note.line_width));

			-- angle
			log (et_libraries.to_string (note.orientation));

			-- visible
			log ("visible " & to_lower(et_libraries.type_text_visible'image (note.visible)));

			-- alignment
			log ("alignment (hor/vert) "
				& to_lower(et_libraries.type_text_alignment_horizontal'image(note.alignment.horizontal))
				& "/"
				& to_lower(et_libraries.type_text_alignment_vertical'image(note.alignment.vertical)));

		end if;
		
		log_indentation_down;
		log_indentation_down;
	end write_note_properties;
	
	procedure write_component_properties ( component : in type_components.cursor) is
	-- Writes the properties of the component indicated by the given cursor.
		use et_string_processing;
	begin
		log_indentation_up;
		
		-- reference (serves as key in list of components)
		log ("component " & to_string (type_components.key(component)) & " properties");

		log_indentation_up;
		
		-- CS: library file name
		-- name in library
		log ("name in library "
			& et_libraries.to_string (type_components.element(component).name_in_library));
		
		-- value
		log ("value "
			& et_libraries.type_component_value.to_string (type_components.element(component).value));

		-- commissioned
		log ("commissioned "
			& string (type_components.element(component).commissioned));

		-- updated
		log ("updated      "
			& string (type_components.element(component).updated));

		-- author
		log ("author "
			& et_libraries.type_person_name.to_string (type_components.element(component).author));
		
		-- appearance
		log (to_string (type_components.element(component).appearance));

		-- depending on the component appearance there is more to report:
		case type_components.element(component).appearance is
			when sch_pcb =>

				-- package variant
				log (et_libraries.to_string (type_components.element(component).variant.variant));
				-- NOTE: This displays the type_component_variant (see et_libraries.ads).
				-- Do not confuse with type_variant (see et_schematic.ads) which also contains the variant name
				-- like in TL084D or TL084N.

				-- datasheet
				log ("datasheet "
					& et_libraries.type_component_datasheet.to_string (type_components.element(component).datasheet));

				-- partcode
				log ("partcode "
					& et_libraries.type_component_partcode.to_string (type_components.element(component).partcode));
				
				-- function
				log ("purpose "
					& et_libraries.type_component_purpose.to_string (type_components.element(component).purpose));
				
			when pcb => null; -- CS
			when others => null; -- CS should never happen as virtual components do not have a package
		end case;

		log_indentation_down;
		log_indentation_down;
		
	end write_component_properties;

	function component_reference (cursor : in type_components.cursor) 
		return type_component_reference is
	-- Returns the component reference where cursor points to.
	begin
		return type_components.key (cursor);
	end component_reference;

	function component_name_in_library (cursor : in type_components.cursor) 
		return et_libraries.type_component_name.bounded_string is
	-- Returns the generic name of a component as it is listed in a library.
	-- The cursor must point to the component in question.
	begin
		return type_components.element (cursor).name_in_library;
	end component_name_in_library;


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
		use et_string_processing;

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
					ports		: in out type_base_ports.list) is
					use et_coordinates;
				begin
					type_base_ports.append (
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
				new_item	=> type_base_ports.empty_list,
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
				if component_cursor_lib = et_libraries.type_components.no_element then
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



	
	procedure write_unit_properties (unit : in type_units.cursor) is
	-- Writes the properties of the unit indicated by the given cursor.
		use et_string_processing;
	begin
		log_indentation_up;
		
		-- unit name
		log ("unit " 
			& et_libraries.type_unit_name.to_string (type_units.key(unit)) & " properties");

		log_indentation_up;
		
		-- alternative representation
		log ("alternative (deMorgan) representation " 
			& to_lower (et_schematic.type_alternative_representation'image (type_units.element(unit).alt_repres)));

		-- timestamp
		log ("timestamp " 
			& string (type_units.element (unit).timestamp));

		-- position
		log (et_schematic.to_string (type_units.element(unit).position));

		-- placeholders
		log ("placeholders");
		log_indentation_up;

			-- reference
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).reference);

			-- value
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).value);

			-- some placeholders exist depending on the component appearance
			case type_units.element(unit).appearance is
				when sch_pcb =>
					
					-- package/footprint
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).packge);

					-- datasheet
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).datasheet);

					-- purpose
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).purpose);
					
					-- partcode
					et_libraries.write_placeholder_properties (
						placeholder => type_units.element(unit).partcode);

				when others => null;
			end case;

			-- commissioned
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).commissioned);

			-- updated
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).updated);

			-- author
			et_libraries.write_placeholder_properties (
				placeholder => type_units.element(unit).author);

		log_indentation_down;
		log_indentation_down;
		log_indentation_down;		
	end write_unit_properties;

	procedure write_coordinates_of_segment (segment : in type_net_segment) is
	-- Writes the start and end coordinates of a net segment.
		use et_string_processing;
		use et_libraries;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		
		log ("start "
			& to_string (et_libraries.type_coordinates (segment.coordinates_start))
			& " end " 
			& to_string (et_libraries.type_coordinates (segment.coordinates_end)),
			level => log_threshold
			);
		
		log_indentation_down;
	end write_coordinates_of_segment;

	procedure write_coordinates_of_junction (junction : in type_net_junction) is
	-- Writes the coordinates of a net junction.
		use et_string_processing;
		use et_libraries;
	
		log_threshold : type_log_level := 1;
	begin
		log_indentation_up;
		
		log (to_string (et_libraries.type_coordinates (junction.coordinates)),
			 level => log_threshold
			); 
		
		log_indentation_down;
	end write_coordinates_of_junction;			


	function junction_sits_on_segment (
	-- Returns true if the given junction sits on the given net segment.
		junction	: in type_net_junction;
		segment		: in type_net_segment'class) 
		return boolean is
		point 		: et_schematic.type_coordinates := junction.coordinates;
		line_start 	: et_schematic.type_coordinates := segment.coordinates_start;
		line_end 	: et_schematic.type_coordinates := segment.coordinates_end;
		zero 		: constant et_libraries.type_grid := 0.0;
		sits_on_segment : boolean := false;
		d : et_geometry.type_distance_point_from_line;

		use et_libraries;
		use et_geometry;
		use et_coordinates;
	begin
		-- calculate the shortes distance of point from line.
		d := distance_of_point_from_line (
			point 		=> et_libraries.type_coordinates(point),
			line_start	=> et_libraries.type_coordinates(line_start),
			line_end	=> et_libraries.type_coordinates(line_end),
			line_range	=> inside_end_points);
		
		if (not d.out_of_range) and d.distance = zero then
			sits_on_segment := true;
		end if;
		return sits_on_segment;
	end junction_sits_on_segment;
	
	
	function to_component_reference (
	-- Converts a string like "IC303" to a composite type_component_reference.
	-- If allow_special_character_in_prefix is given true, the first character
	-- is allowed to be a special character.
	-- NOTE: Leading zeroes in the id are removed.
		text_in : in string;
		allow_special_character_in_prefix : in boolean := false		
		) return type_component_reference is

		r : type_component_reference := (
				prefix => type_component_prefix.to_bounded_string(""),
				id => 0,
				id_width => 1);
	
		c : character;
		p : type_component_prefix.bounded_string;
	
		procedure invalid_reference is
		begin
			et_string_processing.write_message(
				file_handle => current_output,
				text => latin_1.lf & et_string_processing.message_error & "invalid component reference '" & text_in & "'",
				console => true);
			
			raise constraint_error;
		end invalid_reference;

		d : positive;
		digit : natural := 0;

		use et_libraries.type_component_prefix;
	begin
		-- assemble prefix
		for i in text_in'first .. text_in'last loop
			c := text_in(i);
			
			case i is 
				-- The first character MUST be an upper case letter.
				-- If allow_special_charater_in_prefix then the first letter is
				-- allowed to be a special character. (kicad uses '#' for power symbols)
				when 1 => 
					case allow_special_character_in_prefix is
						when false =>
							if is_upper(c) then
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;

						when true =>
							if is_upper(c) or is_special(c) then
								r.prefix := r.prefix & c;
							else 
								invalid_reference;
							end if;
					end case;
					
				-- Further characters are appended to prefix if they are upper case letters.
				-- If a upper-case-letter is found, the prefix is assumed as complete.
				-- A lower case letter will later be detetect when assembling the component id.
				when others =>
					if is_upper(c) then
						r.prefix := r.prefix & c;
					else
						--put("   prefix " & type_component_prefix.to_string(r.prefix));
						-- CS: check if allowed prefix
						d := i; -- d holds the position of the charcter after the prefix.
							-- d is requried when reading the component id. see below.
						exit;
					end if;
			end case;
		end loop;

		-- assemble id
		-- Start with the last character in text_in.
		-- Finish at the position d (that is the first digit after the last letter, see above).
		-- All the characters within this range must be digits.
		-- The significance of the digit is increased after each pass.
		for i in reverse d .. text_in'last loop
			c := text_in(i);
			
			if is_digit(c) then
				r.id := r.id + 10**digit * natural'value(1 * c);
			else
				invalid_reference;
			end if;

			digit := digit + 1; -- increase digit significance (10**0, 10**1, ...)
		end loop;

		-- Set the id width.
		-- It is the number of digits processed when the id was assembled (see above).
		-- Example: if the given string was IC002 then digit is 3.
		r.id_width := digit;
		
-- 		put_line(" id    " & natural'image(r.id));
-- 		put_line(" digits" & natural'image(r.id_width));
		
		return r;
	end to_component_reference;

	function to_string ( reference : in type_component_reference) return string is
	-- Returns the given component reference as string.
	-- Prepends leading zeros according to reference.id_width.
		id_width_wanted	: natural := reference.id_width;
	
		-- The width of the given id is obtained by converting the id to a string
		-- and then by measuring its length:
		id_width_given	: natural := trim(natural'image(reference.id),left)'length;

		-- Finally the number of zeros to prepend is the difference of wanted 
		-- and given digits:
		lz				: natural := id_width_wanted - id_width_given;
	begin
		case lz is
			when 0 => -- no leading zeroes
				return (type_component_prefix.to_string(reference.prefix) 
					& trim(natural'image(reference.id),left));
				
			when others => -- leading zeros required
				return (type_component_prefix.to_string(reference.prefix) 
					& lz * '0' & trim(natural'image(reference.id),left));
		end case;
	end to_string;


	function compare_reference ( left, right : in type_component_reference) return boolean is
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.
	-- CS: needs verification !
		result : boolean := false;
		use et_libraries.type_component_prefix;
	begin
		-- First we compare the prefix.
		-- Example: If left is C201 and right is R4 then the result is true as C comes before R.

		if left.prefix < right.prefix then -- like C201 and R4
			result := true;
		elsif left.prefix > right.prefix then -- like R4 and C201
			result := false;
		elsif left.prefix = right.prefix then -- like IC33 and IC34

			-- If equal prefixes, we compare the id:
			if left.id < right.id then -- like 33 and 34
				result := true;
			else
				result := false; -- like 34 and 33
			end if;

		end if;

		-- in case of equivalence of left and right, we return false (default)
		return result;
	end compare_reference;

	function equal_reference (left, right : in type_component_reference) return boolean is
	-- Returns true if left equals right.
	-- Example: if IC4 = IC4 then return true.
		result : boolean := false;
		use et_libraries.type_component_prefix;
	begin
		-- First we compare the prefix. If prefixes are equal, we compare the id.
		-- If either of them does not match, the result is set false.
		if left.prefix = right.prefix then -- like IC and IC

			if left.id = right.id then -- like 4 and 4
				result := true;
			else -- like 5 and 6
				result := false;
			end if;
			
		else -- like R and IC
			result := false; 
		end if;

		return result;
	end equal_reference;
	


	
	
	function compare_ports (left, right : in type_port) return boolean is
	-- Returns true if left comes before right. Compares by component name and pin name.
	-- If left equals right, the return is false.	
	-- CS: needs verification !
		result : boolean := false;
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
	

	
	procedure add_module (
	-- Adds a module into the rig. Leaves module_cursor pointing
	-- to the module inserted last.
		module_name : in type_submodule_name.bounded_string;
		module		: in type_module) is
		
		inserted : boolean := false;
	begin
		rig.insert (
			key			=> module_name,
			new_item	=> module,
			position	=> module_cursor,
			inserted	=> inserted
			);

		if not inserted then
			null; -- CS: error message
			raise constraint_error;
		end if;
	end add_module;


	procedure add_gui_submodule (
	-- Inserts a gui submodule in the module (indicated by module_cursor)
		name		: in et_schematic.type_submodule_name.bounded_string;
		gui_sub_mod	: in et_schematic.type_gui_submodule) is

		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is
			
			inserted	: boolean := false;
			cursor		: type_gui_submodules.cursor;

			use et_string_processing;
		begin
			module.submodules.insert (
				key			=> name,
				new_item	=> gui_sub_mod,
				position	=> cursor, -- updates cursor. no further meaning
				inserted	=> inserted
				);

			if inserted then
				if log_level >= 1 then
					null; -- CS: write this procedure:
					--et_schematic.write_gui_submodule_properties (gui_sub_mod => cursor);
				end if;
			else -- not inserted. net already in module -> abort
				null; -- CS: 
				raise constraint_error;
			end if;
		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_gui_submodule;

	
	procedure add_sheet_header (
	-- Inserts a sheet header in the module (indicated by module_cursor).
		header	: in type_sheet_header;
		sheet	: in type_schematic_file_name.bounded_string) is

		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.sheet_headers.insert (
				key			=> sheet,
				new_item	=> header);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_header
			end if;

		end add;

	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_sheet_header;

	
	procedure add_frame (
	-- Inserts a drawing frame in the the module (indicated by module_cursor).
	-- As drawing frames are collected in a simple list, the same frame
	-- can be added multiple times.
		frame	: in et_schematic.type_frame) is
		
		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.frames.append (
				new_item	=> frame);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_frame_properties
			end if;

		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_frame;

	
	
	procedure add_title_block (
	-- Inserts a title block in the the module (indicated by module_cursor).
	-- As title blocks are collected in a simple list, the same title block
	-- can be added multiple times.
		tblock	: in et_schematic.type_title_block) is
		
		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.title_blocks.append (
				new_item	=> tblock);

			if log_level >= 1 then
				null; -- CS: write this procedure:
				--et_schematic.write_title_block_properties
			end if;

		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_title_block;


	procedure add_note (
	-- Inserts a note in the the module (indicated by module_cursor).
	-- As notes are collected in a simple list, the same note
	-- can be added multiple times.
		note	: in et_schematic.type_note) is
		
		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is

			use et_string_processing;
		begin
			module.notes.append (
				new_item	=> note);

			if log_level >= 1 then
				null; -- CS: 
				--et_schematic.write_note_properties
			end if;

		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_note;

	
	procedure add_net (
	-- Adds a net into the the module (indicated by module_cursor).
		name	: in et_schematic.type_net_name.bounded_string;
		net		: in et_schematic.type_net) is
		
		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is
			
			inserted	: boolean := false;
			cursor		: type_nets.cursor;

			use et_string_processing;
		begin
			module.nets.insert (
				key			=> name,
				new_item	=> net,
				position	=> cursor, -- updates cursor. no further meaning
				inserted	=> inserted
				);

			if inserted then
				if log_level >= 1 then
					null; -- CS: write this procedure:
					--et_schematic.write_net_properties (net => cursor);
				end if;
			else -- not inserted. net already in module -> abort
				null; -- CS: 
				raise constraint_error;
			end if;
		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_net;

	procedure add_portlists (
	-- Adds the portlists into the module (indicated by module.cursor)
		portlists : in et_schematic.type_portlists.map) is

		procedure add (
			mod_name	: in type_submodule_name.bounded_string;
			module		: in out type_module) is
		begin
			module.portlists := portlists;
		end add;
			
	begin
		rig.update_element (
		position	=> module_cursor,
		process		=> add'access
		);
	end add_portlists;
	
	procedure add_port (
	-- Adds a port to a net in the current module (indicated by module_cursor).
		net		: in et_schematic.type_net_name.bounded_string;
		port	: in et_schematic.type_port ) is

		procedure add (
			net_name	: in type_net_name.bounded_string;
			net			: in out type_net) is

			inserted	: boolean := false;
			cursor		: type_ports.cursor;

			use et_string_processing;
		begin
			net.ports.insert (
				new_item	=> port,
				position	=> cursor, -- updates cursor. no further meaning
				inserted	=> inserted
				);

			if inserted then -- fine. port was inserted successfully
				if log_level >= 1 then				
					--CS : write_port_properties (unit => cursor);
					null;
				end if;
			else -- not inserted, port already in net -> failure
				log_indentation_reset;
				log (
					text => message_error & "multiple occurence of a port in the same net !",
					console => true);
				raise constraint_error;
			end if;
		end add;
		
		procedure locate_net (
			name	: in type_submodule_name.bounded_string;
			module	: in out type_module) is
			
			cursor : type_nets.cursor;
		begin
			cursor := module.nets.find (net);
			-- CS: do something if net not found
			
			module.nets.update_element (
				position	=> cursor,
				process		=> add'access
				);
		end locate_net;

	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> locate_net'access
			);
	end add_port;


	
	procedure add_component (
	-- Adds a component into the the module (indicated by module_cursor).
	-- If a component is already in the list, nothing happens.
	-- Components may occur multiple times in the schematic if they 
	-- consist of more than one unit.
	-- CS: This assumption may not apply for all CAE systems. Currently we
	-- consider only kicad. In other cases the "inserted" check (see below) 
	-- must be enabled via an argument.
		reference	: in et_libraries.type_component_reference;
		component	: in type_component) is
		
		procedure add (
			name	: in type_submodule_name.bounded_string;
			module	: in out type_module) is
			
			inserted	: boolean := false;
			cursor		: type_components.cursor;

			use et_string_processing;
		begin
			module.components.insert (
				key			=> reference,
				new_item	=> component,
				position	=> cursor, -- updates cursor. no further meaning
				inserted	=> inserted
				);

-- 			if inserted then -- first occurence of component
				if log_level >= 1 then
					et_schematic.write_component_properties (component => cursor);
				end if;
-- 			else -- not inserted
-- 				null; -- CS: see comment above
				--raise constraint_error;
-- 			end if;
		end add;
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> add'access
			);
	end add_component;
	
	procedure add_unit ( -- CS: comments
		reference	: in et_libraries.type_component_reference;
		unit_name	: in et_libraries.type_unit_name.bounded_string;
		unit 		: in type_unit) is

		procedure add (
			reference	: in et_libraries.type_component_reference;
			component	: in out type_component) is

			inserted	: boolean := false;
			cursor		: type_units.cursor;

			use et_string_processing;
		begin
			component.units.insert (
				key			=> unit_name,
				new_item	=> unit,
				position	=> cursor, -- updates unit_cursor. no further meaning
				inserted	=> inserted
				);

			if inserted then -- fine. unit was inserted successfully
				if log_level >= 1 then				
					write_unit_properties (unit => cursor);
				end if;
			else -- not inserted, unit already in component -> failure
				log_indentation_reset;
				log (
					text => message_error & "multiple occurence of the same unit !",
					console => true);
				raise constraint_error;
			end if;
		end add;
		
		procedure locate_component (
			name	: in type_submodule_name.bounded_string;
			module	: in out type_module) is
			
			cursor : type_components.cursor;
		begin
			cursor := module.components.find (reference);
			-- CS: do something if reference not found
			
			module.components.update_element (
				position	=> cursor,
				process		=> add'access
				);
		end locate_component;
		
	begin
		rig.update_element (
			position	=> module_cursor,
			process		=> locate_component'access
			);
	end add_unit;

	procedure reset_component_cursor (cursor : in out type_components.cursor) is
	-- Resets the given component cursor to the begin of the component list.
		procedure reset (
			name	: in type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_components;
		begin
			cursor := first (module.components);
		end reset;
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> reset'access
			);
	end reset_component_cursor;

	procedure reset_library_cursor (cursor : in out type_full_library_names.cursor) is
	-- Resets the given library cursor to the begin of the library list list.
		procedure reset (
			name	: in type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_full_library_names;
		begin
			cursor := first (module.libraries);
		end reset;
	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> reset'access
			);
	end reset_library_cursor;

	function number_of_libraries return count_type is
	-- Returns the number of project libraries.
		n : count_type := 0;

		procedure get (
			name	: in type_submodule_name.bounded_string;
			module	: in type_module) is
			use type_full_library_names;
		begin
			n := length (module.libraries);
		end get;

	begin
		type_rig.query_element (
			position	=> module_cursor,
			process		=> get'access
			);

		return n;
	end number_of_libraries;

	function units_of_component (component_cursor : in type_components.cursor) return type_units.map is
	-- Returns the units of the given component.
		u : type_units.map;

		procedure locate (
			name : in type_component_reference;
			component : in type_component) is
		begin
			-- copy the units of the component to the return value
			u := component.units;
		end locate;
		
	begin
		-- locate the given component by component_cursor
		type_components.query_element (component_cursor, locate'access);
		
		-- CS: do something if cursor invalid. via exception handler ?
		return u;
	end units_of_component;


-- 	function position_of_unit (
-- 	-- Returns the coordinates of the given unit.
-- 	-- The unit is one element in the given list of units.
-- 		name : in type_unit_name.bounded_string; -- the unit being inquired
-- 		units : in type_units.map) -- the list of units
-- 		return type_coordinates is
-- 
-- 		unit_cursor : type_units.cursor;
-- 		c : type_coordinates;
-- 	begin
-- 		unit_cursor := type_units.find (container => units, key => name);
-- 
-- 		return c; --element (unit_cursor).coordinates;
-- 	end;

	
-- 	function get_component_reference (cursor : in out type_components.cursor)
-- 	-- Returns the component reference where the component cursor points to.
-- 		return type_component_reference is
-- 
-- 		cr : type_component_reference;
-- 		
-- 		procedure get (
-- 			name	: in type_submodule_name.bounded_string;
-- 			module	: in type_module) is
-- 			use type_components;
-- 		begin
-- 			cr := key (cursor);
-- 		end get;
-- 		
-- 	begin
-- 		type_rig.query_element (
-- 			position	=> module_cursor,
-- 			process		=> get'access
-- 			);
-- 
-- 		return cr;
-- 	end get_component_reference;


	procedure warning_on_name_less_net (
	-- Writes a warning about a name-less net.
		name 	: in et_schematic.type_net_name.bounded_string;
		net		: in et_schematic.type_net
		) is

		use et_string_processing;
	begin
		log (
			text => message_warning & "name-less net " & to_string (name) & " found !"
			);
		-- CS: output coordinates of net (lowest x/Y)
	end warning_on_name_less_net;
	
	
end et_schematic;
-- Soli Deo Gloria
