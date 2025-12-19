------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             MODULE READ                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- ToDo:
-- - clean up
-- - simplify code
-- - separate schematic and board stuff in et_module_write.board/schematic
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;
with ada.directories;				use ada.directories;
with ada.containers;

with et_text;						use et_text;
with et_schematic_geometry;
with et_schematic_coordinates;

with et_section_headers;			use et_section_headers;
with et_keywords;					use et_keywords;
with et_module_sections;			use et_module_sections;
with et_pcb_sides;
with et_board_geometry;
with et_board_coordinates;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_primitive_objects;			use et_primitive_objects;
with et_axes;						use et_axes;
with et_module_instance;			use et_module_instance;
with et_nets;
with et_route;						use et_route;
with et_net_strands;				use et_net_strands;
with et_net_names;					use et_net_names;
with et_net_junction;
with et_net_ports;
with et_net_segment;
with et_net_labels;
with et_net_connectors;
with et_net_class;
with et_net_class_name;
with et_net_class_description;
with et_net_classes;
with et_port_names;
with et_symbol_ports;
with et_device_name;				use et_device_name;

with et_design_rules_board;			use et_design_rules_board;

with et_device_model;
with et_device_appearance;
with et_device_purpose;
with et_device_value;
with et_device_partcode;
with et_device_sections;
with et_symbol_read;
with et_schematic_text;
with et_device_read;
with et_drawing_frame;
with et_drawing_frame.schematic;
with et_drawing_frame_rw;
with et_sheets;
with et_pcb;
with et_pcb_stack;

with et_board_read;					use et_board_read;
with et_package_sections;

with et_drills;
with et_vias;
with et_terminals;

with et_conventions;

with et_time;

with et_schematic_ops;
with et_schematic_ops.units;
with et_schematic_ops.submodules;
with et_board_ops;
with et_board_ops.ratsnest;

with et_schematic_text;
with et_board_text;
with et_board_layer_category;

with et_submodules;

with et_netlists;

with et_conductor_segment.boards;
with et_fill_zones;
with et_fill_zones.boards;
with et_thermal_relief;
with et_conductor_text.boards;
with et_route_restrict.boards;
with et_via_restrict.boards;
with et_stopmask;
with et_stencil;
with et_silkscreen;
with et_assy_doc;
with et_keepout;
with et_pcb_contour;
with et_pcb_placeholders;
with et_mirroring;						use et_mirroring;
with et_directory_and_file_ops;
with et_alignment;						use et_alignment;
with et_object_status;
with et_string_processing;				use et_string_processing;
with et_general_rw;						use et_general_rw;
with et_exceptions;						use et_exceptions;

with et_module_read_device_electrical;		use et_module_read_device_electrical;
with et_module_read_device_non_electrical;	use et_module_read_device_non_electrical;

with et_module_read_meta;				use et_module_read_meta;
with et_module_read_assembly_variant;	use et_module_read_assembly_variant;

with et_module_read_design_rules;		use et_module_read_design_rules;
with et_module_read_grid;				use et_module_read_grid;
with et_module_read_net_classes;		use et_module_read_net_classes;


package body et_module_read is

	use pac_generic_modules;

	
	procedure read_module (
		file_name 		: in string; -- motor_driver.mod, templates/clock_generator.mod
		log_threshold	: in type_log_level) 
	is
		use et_schematic_ops; -- CS place it where really needed
		
		
		previous_input : ada.text_io.file_type renames current_input;
		
		-- Environment variables like $templates could be in file name.
		-- In order to test whether the given module file exists, file name_name must be expanded
		-- so that the environment variables are replaced by the real paths like:
		-- templates/clock_generator.mod or
		-- /home/user/et_templates/pwr_supply.mod.
		use et_directory_and_file_ops;
		file_name_expanded : constant string := expand (file_name);
			
		file_handle : ada.text_io.file_type;
		use pac_generic_modules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being read
		module_inserted : boolean;

		-- The line read from the the module file:
		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the module. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 11;
		package stack is new et_general_rw.stack_lifo (
			item	=> type_module_section,
			max 	=> max_section_depth);


		

		

		
		
		function to_size (
			line : in type_fields_of_line; -- "size x 30 y 40"
			from : in type_field_count_positive)
			return et_submodules.type_submodule_size 
		is
			use et_schematic_geometry.pac_geometry_2;
			use ada.containers;
			
			size : et_submodules.type_submodule_size; -- to be returned
			place : type_field_count_positive := from; -- the field being read from given line

			-- CS: flags to detect missing x or y
		begin
			while place <= get_field_count (line) loop

				-- We expect after the x the corresponding value for x
				if f (line, place) = keyword_x then
					size.x := to_distance (f (line, place + 1));

				-- We expect after the y the corresponding value for y
				elsif f (line, place) = keyword_y then
					size.y := to_distance (f (line, place + 1));

				else
					invalid_keyword (f (line, place));
				end if;
					
				place := place + 2;
			end loop;
			
			return size;
		end to_size;





		
		

		
		


		
		
	-- NETS:
		
		net_name	: pac_net_name.bounded_string; -- motor_on_off
		net			: et_nets.type_net;


		-- Reads a line that describes a net like "name AGND", "class default"
		-- or "scope  local":
		procedure read_net is
			kw : constant string := f (line, 1);
			use ada.containers;
			use et_net_class;
			use et_net_class_name;
		begin
			log (text => "read net", level => log_threshold + 1);
			log_indentation_up;

			
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_name then
				expect_field_count (line, 2);
				net_name := to_net_name (f (line,2));
				
			elsif kw = keyword_class then
				-- CS: imported kicad projects lack the class name sometimes.
				-- For this reason we do not abort in such cases but issue a warning.
				-- If abort is a must, the next two statements are required. 
				-- The "if" construct must be in comments instead.
				-- It is perhaps more reasonable to care for this flaw in et_kicad_pcb package.
				
				-- expect_field_count (line, 2);
				-- net.class := et_pcb.to_net_class_name (f (line,2));
				
				if get_field_count (line) = 2 then
					net.class := to_net_class_name (f (line,2));
				else
					net.class := net_class_name_default;
					log (text => message_warning & get_affected_line (line) 
						& "No net class specified ! Assume default class !");
				end if;
				
			elsif kw = keyword_scope then
				expect_field_count (line, 2);
				net.scope := et_netlists.to_net_scope (f (line,2));
				
			else
				invalid_keyword (kw);
			end if;

			log_indentation_down;
		end read_net;



		
		-- Assigns the net to the module:
		procedure assign_net is 

			procedure insert_net (
				module_name	: in pac_module_name.bounded_string;
				module		: in out type_generic_module)
			is
				use et_nets;
				use pac_net_name;
				inserted : boolean;
				cursor : pac_nets.cursor;
			begin
				log (text => "net " & to_string (net_name), level => log_threshold + 2);

				-- CS: notify about missing parameters (by reading the parameter-found-flags)
				-- If a parameter is missing, the default is assumed. See type_net spec.
				
				pac_nets.insert (
					container	=> module.nets,
					key			=> net_name,
					new_item	=> net,
					inserted	=> inserted,
					position	=> cursor);

				if not inserted then
					log (ERROR, "net " & to_string (net_name) & " already exists !");
					raise constraint_error;
				end if;
			end insert_net;

			
		begin
			log (text => "assign net", level => log_threshold + 1);
			log_indentation_up;
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> insert_net'access);

			-- clean up for next net
			net_name := to_net_name ("");
			net := (others => <>);
			
			log_indentation_down;
		end assign_net;
		


		

		
	-- NET STRANDS:
		
		strands : pac_strands.list;
		strand	: type_strand;


		-- Reads a line that describes the position of a strand:
		procedure read_strand is
			kw : constant string := f (line, 1);
		begin
			log (text => "read strand", level => log_threshold + 1);
			log_indentation_up;

			
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position sheet 1 x 1.000 y 5.555
				expect_field_count (line, 7);

				-- extract strand position starting at field 2
				strand.position := to_strand_position (line, 2, log_threshold + 2);
			else
				invalid_keyword (kw);
			end if;

			log_indentation_down;
		end read_strand;
		



		-- Assigns the strand to the list of strands:
		procedure assign_net_strand is
			use et_schematic_geometry;
			use pac_geometry_2;
			use et_schematic_coordinates;
			use et_sheets;
			use pac_net_name;
			position_found_in_module_file : type_vector_model := strand.position.place;
		begin
			log (text => "assign strand", level => log_threshold + 1);
			log_indentation_up;
			
			-- Calculate the lowest x/y position and set sheet number of the strand
			-- and overwrite previous x/y position. 
			-- So the calculated position takes precedence over the position found in 
			-- the module file.
			set_strand_position (strand);

			-- Issue warning about this mismatch:
			if strand.position.place /= position_found_in_module_file then
				
				log (WARNING, get_affected_line (line) 
						& "Sheet" & to_string (get_sheet (strand.position))
						& " net " 
						& to_string (net_name) & ": Lowest x/y position of strand invalid !");
				
				log (text => " Found " & to_string (position_found_in_module_file));
				log (text => " Will be overridden by calculated position " & 
						to_string (strand.position.place));
			end if;
								
			-- insert strand in collection of strands
			pac_strands.append (
				container	=> strands,
				new_item	=> strand);

			-- clean up for next single strand
			strand := (others => <>); 

			log_indentation_down;
		end assign_net_strand;


		


		
	-- NET SEGMENTS:
		
		net_segments	: et_net_segment.pac_net_segments.list;
		net_segment		: et_net_segment.type_net_segment;

		
		procedure read_net_segment is
			use et_symbol_read;
			kw : constant string := f (line, 1);

			use et_net_segment;
			use et_schematic_geometry.pac_geometry_2;
			vm : type_vector_model;
		begin
			log (text => "read net segment", level => log_threshold + 1);
			log_indentation_up;

			
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_start then -- "start x 3 y 4"
				expect_field_count (line, 5);

				-- extract start position starting at field 2
				vm := to_vector_model (line, from => 2);
				set_A (net_segment, vm);
				
			elsif kw = keyword_end then -- "end x 6 y 4"
				expect_field_count (line, 5);

				-- extract end position starting at field 2
				vm := to_vector_model (line, from => 2);
				set_B (net_segment, vm);

			else
				invalid_keyword (kw);
			end if;

			log_indentation_down;
		end read_net_segment;




		-- Appends the net_segment to the list of net segments:
		procedure assign_net_segment is 
			use et_net_segment;
		begin
			log (text => "assign net segment", level => log_threshold + 1);
			log_indentation_up;

			
			et_net_segment.pac_net_segments.append (
				container	=> net_segments,
				new_item	=> net_segment);

			-- Clean up for next net segment:
			reset_net_segment (net_segment);

			log_indentation_down;
		end assign_net_segment;



		
		

	-- NET JUNCTIONS:
		
		net_junctions	: et_net_junction.type_junctions;

		-- Reads a line that describes a net junction
		-- (like "A/B") that is active on an end
		-- of a net segment. Once the line is read, the junction
		-- is activated in variable net_junctions (see above):
		procedure read_net_junction is
			use et_net_junction;
			use et_schematic_geometry.pac_geometry_2;

			AB_end : type_start_end_point;
			error : boolean := false;
		begin
			log (text => "read net junction", level => log_threshold + 1);
			log_indentation_up;
			
			-- There must be only a single field in the line:
			expect_field_count (line, 1);
			
			-- Read the targeted end point of the segment:
			AB_end := to_start_end_point (f (line, 1)); -- A/B

			log (text => "end " & to_string (AB_end), level => log_threshold + 2);

			-- CS:
			-- Make a junction:
			-- make_net_junction (
			-- 	arguments	=> AB_end,
			-- 	error		=> error,
			-- 	connector	=> connector);

			if not error then
				-- log (text => "direction " & get_direction (connector), level => log_threshold + 2);
				
				-- Activate the junction on the targeted end of the segment:
				case AB_end is
					when A => net_junctions.A := true;
					when B => net_junctions.B := true;
				end case;
			end if;

			-- CS handle error
			
			log_indentation_down;
		end read_net_junction;


		

		-- Assigns the net_junctions to the net:
		procedure assign_net_junctions is begin
			log (text => "assign net junctions", level => log_threshold + 1);
			log_indentation_up;
			
			-- Assign the net_junctions to the net segment:
			net_segment.junctions := net_junctions;

			if net_segment.junctions.A then
				log (text => "A", level => log_threshold + 2);
			end if;

			if net_segment.junctions.B then
				log (text => "B", level => log_threshold + 2);
			end if;
			
			-- clean up for next junctions
			net_junctions := (others => <>);
			
			log_indentation_down;
		end assign_net_junctions;




		

	-- NET CONNECTORS:

		net_connectors	: et_net_connectors.type_net_connectors;
		

		-- Reads a line that describes a net connector 
		-- (like "A/B direction input/output") that is attached to an end
		-- of a net segment. Once the line is read, the connector
		-- is assigned to the variable net_connectors (see above):
		procedure read_net_connector is
			use et_net_connectors;
			use et_schematic_geometry.pac_geometry_2;

			AB_end : type_start_end_point;
			error : boolean := false;
			connector : type_net_connector (active => true);
		begin
			log (text => "read net connector", level => log_threshold + 1);
			log_indentation_up;
			
			-- There must be 3 fields in the line:
			expect_field_count (line, 3);
			
			-- Read the targeted end point of the segment:
			AB_end := to_start_end_point (f (line, 1)); -- A/B

			log (text => "end " & to_string (AB_end), level => log_threshold + 2);
			
			-- Make a net connector from the fields 2 and 3:
			make_net_connector (
				arguments	=> remove_field (line, 1, 1),
				error		=> error,
				connector	=> connector);

			if not error then
				log (text => "direction " & get_direction (connector), level => log_threshold + 2);
				
				-- Assign the connector to the targeted end of the segment:
				case AB_end is
					when A => net_connectors.A := connector;
					when B => net_connectors.B := connector;
				end case;
			end if;

			-- CS handle error
			
			log_indentation_down;
		end read_net_connector;




		-- Assigns the net_connectors to the net_segment:
		procedure assign_net_connectors is 
			use et_net_connectors;
		begin
			log (text => "assign net connectors", level => log_threshold + 1);
			log_indentation_up;
			
			-- Assign the net connectors to the net segment:
			net_segment.connectors := net_connectors;

			log (text => "A " & to_string (net_segment.connectors.A), level => log_threshold + 2);
			log (text => "B " & to_string (net_segment.connectors.B), level => log_threshold + 2);
			
			-- clean up for next connectors
			net_connectors := (others => <>);
			
			log_indentation_down;
		end assign_net_connectors;

		

		
		
	-- NET LABELS:
		
		net_labels				: et_net_labels.pac_net_labels.list;
		net_label 				: et_net_labels.type_net_label;
		
		net_label_rotation		: et_text.type_rotation_documentation := 
			et_text.type_rotation_documentation'first;

		-- The net label direction is relevant if it is a tag label:
		net_label_direction : et_net_connectors.type_connector_direction := 
			et_net_connectors.type_connector_direction'first;

		-- CS warn about parameter "direction" being ignored


		-- Reads a line that describes a net label:
		procedure read_label is
			use et_schematic_text;
			use pac_text_schematic;
			
			use et_symbol_read;
			use et_schematic_coordinates;	
			use et_schematic_geometry;
			use pac_geometry_2;
			use et_net_labels;
			kw : constant string := f (line, 1);
		begin
			log (text => "read net label", level => log_threshold + 1);
			log_indentation_up;

			
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 148.59 y 104.59
				expect_field_count (line, 5);

				-- extract label position starting at field 2 of line
				net_label.position := to_vector_model (line, 2);

				
			elsif kw = keyword_rotation then -- rotation 0.0
				expect_field_count (line, 2);
				net_label_rotation := to_rotation_doc (to_rotation (f (line, 2)));

				
			elsif kw = keyword_size then -- size 1.3
				expect_field_count (line, 2);
				net_label.size := to_distance (f (line, 2));
		
			else
				invalid_keyword (kw);
			end if;

			log_indentation_down;		
		end read_label;



		-- Assigns the net_labels to the net segment:
		procedure assign_net_labels is 
			use et_net_labels;
		begin
			log (text => "assign net connectors", level => log_threshold + 1);
			log_indentation_up;
			
			net_segment.labels := net_labels;

			-- clean up for next label collection
			pac_net_labels.clear (net_labels);

			log_indentation_down;
		end assign_net_labels;


		


	-- NET PORTS:
		
		net_device_port : et_net_ports.type_device_port;
		-- net_device_ports : et_net_segment.pac_device_ports.set;

		net_submodule_port : et_net_ports.type_submodule_port;
		-- net_submodule_ports : et_net_segment.pac_submodule_ports.set;

		net_netchanger_port : et_netlists.type_port_netchanger;
		-- net_netchanger_ports : et_netlists.pac_netchanger_ports.set;

		net_segment_ports : et_net_ports.type_ports_AB;
		
		
		-- Reads a port of a device, submodule or netchanger
		-- that is connected with a net segment and sets the variables
		-- net_device_port, net_submodule_port, net_netchanger_port.
		-- Appends the port to net_segment_ports.
		-- NOTE: A device, submodule or netchanger port is defined by a
		-- single line.
		-- Upon reading the line like "A/B device/submodule/netchanger x port 1" 
		-- the port is appended to the corresponding port collection 
		-- net_segment_ports immediately after the line has been read:
		procedure read_net_port is
			use et_module_instance;
			use et_device_model;
			use et_port_names;
			use et_symbol_ports;
			use et_nets;
			use et_net_segment;
			use et_net_ports;
			use pac_net_name;

			use et_schematic_coordinates;
			use et_schematic_geometry;
			use pac_geometry_2;
			
			AB_end : type_start_end_point;
			
			kw : constant string := f (line, 2);

			error : boolean := false;
		begin
			AB_end := to_start_end_point (f (line, 1));
			
			if kw = keyword_device then -- A/B device R1 unit 1 port 1
				expect_field_count (line, 7);

				make_device_port (
					arguments	=> remove_field (line, 1, 1),
					error		=> error,
					port		=> net_device_port);

				if not error then

					-- CS really required ?
					-- Insert port in port collection of device ports. First make sure it is
					-- not already in the net segment.
					-- if pac_device_ports.contains (net_device_ports, net_device_port) then
					-- 	log (ERROR, "device " & to_string (net_device_port.device_name) &
					-- 		" port " & to_string (net_device_port.port_name) & 
					-- 		" already in net segment !", console => true);
					-- 	raise constraint_error;
					-- end if;

					case AB_end is
						when A => net_segment_ports.A.devices.insert (net_device_port); 
						when B => net_segment_ports.B.devices.insert (net_device_port); 
					end case;

					net_device_port := (others => <>);
				else
					invalid_keyword (f (line, 3));
					-- CS: improve this message. show details.
				end if;

				
			elsif kw = keyword_submodule then -- A/B submodule motor_driver port mot_on_off
				expect_field_count (line, 5);
				
				net_submodule_port.module_name := to_instance_name (f (line, 3)); -- motor_driver

				if f (line, 4) = keyword_port then -- port
					net_submodule_port.port_name := to_net_name (f (line, 5)); -- A

					-- CS really required ?
					-- Insert submodule port in collection of submodule ports. First make sure it is
					-- not already in the net segment.
					-- if pac_submodule_ports.contains (net_submodule_ports, net_submodule_port) then
					-- 	log (ERROR, "submodule " & to_string (net_submodule_port.module_name) &
					-- 		" port " & to_string (net_submodule_port.port_name) & 
					-- 		" already in net segment !", console => true);
					-- 	raise constraint_error;
					-- end if;
					
					case AB_end is
						when A => net_segment_ports.A.submodules.insert (net_submodule_port); 
						when B => net_segment_ports.B.submodules.insert (net_submodule_port); 
					end case;
					
					-- clean up for next submodule port
					net_submodule_port := (others => <>);
				else
					invalid_keyword (f (line, 4));
				end if;

				
				
			elsif kw = keyword_netchanger then -- A/B netchanger 1 port master/slave
				expect_field_count (line, 5);
				
				net_netchanger_port.index := et_submodules.to_netchanger_id (f (line, 3)); -- 1

				if f (line, 4) = keyword_port then -- port
					net_netchanger_port.port := et_submodules.to_port_name (f (line, 5)); -- MASTER, SLAVE

					-- CS really required ?
					-- Insert netchanger port in collection of netchanger ports. First make sure it is
					-- not already in the net segment.
					-- if et_netlists.pac_netchanger_ports.contains (net_netchanger_ports, net_netchanger_port) then
					-- 	log (ERROR, "netchanger" & et_submodules.to_string (net_netchanger_port.index) &
					-- 		et_submodules.to_string (net_netchanger_port.port) & " port" & 
					-- 		" already in net segment !", console => true);
					-- 	raise constraint_error;
					-- end if;
					
					-- et_netlists.pac_netchanger_ports.insert (net_netchanger_ports, net_netchanger_port);
					case AB_end is
						when A => net_segment_ports.A.netchangers.insert (net_netchanger_port); 
						when B => net_segment_ports.B.netchangers.insert (net_netchanger_port); 
					end case;
					
					-- clean up for next netchanger port
					net_netchanger_port := (others => <>);
				else
					invalid_keyword (f (line, 4));
				end if;
				
			else
				invalid_keyword (kw);
			end if;
		end read_net_port;


		

		-- Assigns net_segment_ports to the net segment:
		procedure assign_net_ports is begin
			log (text => "assign net ports", level => log_threshold + 1);
			log_indentation_up;
			
			net_segment.ports := net_segment_ports;
			
			-- clean up for next port collections (of another net segment)
			net_segment_ports := (others => <>);

			log_indentation_down;
		end assign_net_ports;

		

		

	-- ROUTE:
		
		route		: type_net_route;

		
		sheet_descriptions			: et_drawing_frame.schematic.pac_schematic_descriptions.map;
		sheet_description_category	: et_drawing_frame.schematic.type_schematic_sheet_category := 
			et_drawing_frame.schematic.schematic_sheet_category_default; -- product/develpment/routing
		
		sheet_description_number	: et_sheets.type_sheet := et_sheets.type_sheet'first; -- 1, 2. 3, ...
		sheet_description_text		: et_text.pac_text_content.bounded_string;		-- "voltage regulator"

		-- CS frame_count_schematic		: et_schematic_coordinates.type_submodule_sheet_number := et_schematic_coordinates.type_submodule_sheet_number'first; -- 10 frames
		frame_template_schematic	: et_drawing_frame.pac_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_1.frs
		frame_template_board		: et_drawing_frame.pac_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_2.frb
		frame_board_position		: et_drawing_frame.type_position; -- x 0 y 0


		
		-- Reads the name of the schematic frame template.
		procedure read_frame_template_schematic is
			use et_drawing_frame_rw;
			use et_drawing_frame;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_template then -- template $ET_FRAMES/drawing_frame_version_1.frs
				expect_field_count (line, 2);
				frame_template_schematic := to_template_name (f (line, 2));
			else
				invalid_keyword (kw);
			end if;
		end;

		

		
		-- Reads the name of the board frame template.
		-- Reads the position of the frame:
		procedure read_frame_template_board is
			use et_drawing_frame;
			use et_drawing_frame_rw;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_template then -- template $ET_FRAMES/drawing_frame_version_2.frb
				expect_field_count (line, 2);
				frame_template_board := to_template_name (f (line, 2));

			elsif kw = keyword_position then -- position x 40 y 60
				expect_field_count (line, 5);
				frame_board_position := et_drawing_frame_rw.to_position (line, 2);
			else
				invalid_keyword (kw);
			end if;
		end;

		
		
		-- Reads the description of a schematic sheet:
		procedure read_sheet_description is
			use et_schematic_coordinates;	
			use et_drawing_frame.schematic;
			use et_sheets;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_sheet_number then -- number 2
				expect_field_count (line, 2);
				sheet_description_number := to_sheet (f (line, 2));

			elsif kw = keyword_sheet_category then -- category develompent/product/routing
				expect_field_count (line, 2);
				sheet_description_category := to_category (f (line, 2));

			elsif kw = keyword_sheet_description then -- text "voltage regulator"
				expect_field_count (line, 2);
				sheet_description_text := to_content (f (line, 2));
				
			else
				invalid_keyword (kw);
			end if;
		end read_sheet_description;

		

		

		schematic_text : et_schematic_text.type_text;
		

		
		
		-- general board stuff
		use et_board_text.pac_text_board;
		board_text : type_text_fab_with_content;
		board_text_placeholder : et_pcb_placeholders.type_text_placeholder;


		-- This procdure reads a property of a general 
		-- placeholder in the board drawing (like project name, material code, ...):
		procedure read_board_text_placeholder is
			use et_board_geometry.pac_geometry_2;
			use et_pcb_placeholders;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
				expect_field_count (line, 7);

				-- extract position of note starting at field 2
				board_text_placeholder.position := to_position (line, 2);

			elsif kw = keyword_size then -- size 1.000
				expect_field_count (line, 2);
				board_text_placeholder.size := to_distance (f (line, 2));

			elsif kw = keyword_linewidth then -- linewidth 0.1
				expect_field_count (line, 2);
				board_text_placeholder.line_width := to_distance (f (line, 2));

			elsif kw = keyword_alignment then -- alignment horizontal center vertical center
				expect_field_count (line, 5);

				-- extract alignment starting at field 2
				board_text_placeholder.alignment := to_alignment (line, 2);
				
			elsif kw = keyword_meaning then -- meaning project_name
				expect_field_count (line, 2);
				board_text_placeholder.meaning := to_meaning (f (line, 2));
				
			else
				invalid_keyword (kw);
			end if;
		end read_board_text_placeholder;

		
		signal_layers : et_pcb_stack.type_signal_layers.set;
		conductor_layer, dielectric_layer : et_pcb_stack.type_signal_layer := et_pcb_stack.type_signal_layer'first;
		conductor_thickness : et_pcb_stack.type_conductor_thickness := et_pcb_stack.conductor_thickness_outer_default;
		dielectric_found : boolean := false;
		board_layer : et_pcb_stack.type_layer;
		board_layers : et_pcb_stack.package_layers.vector;

		-- Whenver a signal layer id is to be read, it must be checked against the
		-- deepest signal layer used. The variable check_layers controls this check.
		-- As preparation we enable the check by setting the "check" to YES.
		-- When section BOARD_LAYER_STACK closes, we also assign the deepest layer used.
		check_layers : et_pcb_stack.type_layer_check (check => et_pcb_stack.YES);

		
		-- Checks the global signal_layer variable against check_layers:
		procedure validate_signal_layer is 
			use et_pcb_stack;
		begin
			-- Issue warning if signal layer is invalid:
			if not signal_layer_valid (signal_layer, check_layers) then
				signal_layer_invalid (line, signal_layer, check_layers);
			end if;
		end validate_signal_layer;

		

		-- Checks a given signal layer against check_layers:
		procedure validate_signal_layer (l : in et_pcb_stack.type_signal_layer) is
			use et_pcb_stack;
		begin
			-- Issue warning if signal layer is invalid:
			if not signal_layer_valid (l, check_layers) then
				signal_layer_invalid (line, l, check_layers);
			end if;
		end validate_signal_layer;


		
		-- temporarily a netchanger is stored here:
		netchanger		: et_submodules.type_netchanger;
		netchanger_id	: et_submodules.type_netchanger_id := et_submodules.type_netchanger_id'first;

		
		procedure read_netchanger is
			use et_schematic_geometry;
			use et_board_geometry.pac_geometry_2;
			use et_schematic_coordinates;	
			kw : constant string := f (line, 1);
			use et_pcb_stack;
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_name then -- name 1, 2, 304, ...
				expect_field_count (line, 2);
				netchanger_id := et_submodules.to_netchanger_id (f (line, 2));
				
			elsif kw = keyword_position_in_schematic then -- position_in_schematic sheet 1 x 1.000 y 5.555
				expect_field_count (line, 7);

				-- extract position (in schematic) starting at field 2
				netchanger.position_sch := to_position (line, 2);

			elsif kw = keyword_rotation_in_schematic then -- rotation_in_schematic 180.0
				expect_field_count (line, 2);
				set_rotation (netchanger.position_sch, pac_geometry_2.to_rotation (f (line, 2)));

			elsif kw = keyword_position_in_board then -- position_in_board x 55.000 y 7.555
				expect_field_count (line, 5);

				-- extract position (in board) starting at field 2
				netchanger.position_brd := to_vector_model (line, 2);

			elsif kw = keyword_layer then -- layer 3 (signal layer in board)
				expect_field_count (line, 2);
				netchanger.layer := et_pcb_stack.to_signal_layer (f (line, 2));
				validate_signal_layer (netchanger.layer);
				
			else
				invalid_keyword (kw);
			end if;
		end read_netchanger;


		
		
		procedure read_cutout_route is
			use et_board_geometry.pac_geometry_2;
			use et_pcb_stack;
			use et_fill_zones;
			kw : constant  string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_easing_style then -- easing_style none/chamfer/fillet
				expect_field_count (line, 2);
				board_easing.style := to_easing_style (f (line, 2));

			elsif kw = keyword_easing_radius then -- easing_radius 0.3
				expect_field_count (line, 2);
				board_easing.radius := to_distance (f (line, 2));

			elsif kw = keyword_layer then -- layer 2
				expect_field_count (line, 2);
				signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
				validate_signal_layer;

			else
				invalid_keyword (kw);
			end if;
		end read_cutout_route;

		

		
		procedure read_cutout_non_conductor is
			use et_board_geometry.pac_geometry_2;
			use et_fill_zones;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_easing_style then -- easing_style none/chamfer/fillet
				expect_field_count (line, 2);													
				board_easing.style := to_easing_style (f (line, 2));

			elsif kw = keyword_easing_radius then -- easing_radius 0.4
				expect_field_count (line, 2);													
				board_easing.radius := to_distance (f (line, 2));
				
			else
				invalid_keyword (kw);
			end if;
		end read_cutout_non_conductor;

		

		
		procedure read_cutout_restrict is
			use et_pcb_stack;
			use et_board_geometry.pac_geometry_2;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_layers then -- layers 1 14 3

				-- there must be at least two fields:
				expect_field_count (line => line, count_expected => 2, warn => false);
				signal_layers := to_layers (line, check_layers);

			else
				invalid_keyword (kw);
			end if;
		end read_cutout_restrict;


		
		
		-- Reads cutout zone in conductor layer.
		-- NOTE: This is about floating conductor zones. Has nothing to
		-- do with nets and routes.
		procedure read_cutout_conductor_non_electric is
			use et_pcb_stack;
			use et_board_geometry.pac_geometry_2;
			use et_fill_zones;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_easing_style then -- easing_style none/chamfer/fillet
				expect_field_count (line, 2);													
				board_easing.style := to_easing_style (f (line, 2));

			elsif kw = keyword_easing_radius then -- easing_radius 0.4
				expect_field_count (line, 2);													
				board_easing.radius := to_distance (f (line, 2));
				
			elsif kw = keyword_layer then -- layer 1
				expect_field_count (line, 2);
				signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
				validate_signal_layer;

			else
				invalid_keyword (kw);
			end if;
		end read_cutout_conductor_non_electric;


		
		
		-- Reads parameters of a conductor fill zone connected with a net:
		procedure read_fill_zone_route is
			use et_board_geometry.pac_geometry_2;
			use et_fill_zones;
			use et_fill_zones.boards;
			use et_thermal_relief;
			use et_pcb_stack;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_priority then -- priority 2
				expect_field_count (line, 2);
				contour_priority := to_priority (f (line, 2));

			elsif kw = keyword_isolation then -- isolation 0.5
				expect_field_count (line, 2);
				polygon_isolation := to_distance (f (line, 2));
				
			elsif kw = keyword_easing_style then -- easing_style none/chamfer/fillet
				expect_field_count (line, 2);
				board_easing.style := to_easing_style (f (line, 2));

			elsif kw = keyword_easing_radius then -- easing_radius 0.3
				expect_field_count (line, 2);
				board_easing.radius := to_distance (f (line, 2));

			elsif kw = keyword_fill_style then -- fill_style solid,hatched
				expect_field_count (line, 2);
				board_fill_style := to_fill_style (f (line, 2));

			elsif kw = keyword_spacing then -- spacing 1
				expect_field_count (line, 2);
				fill_spacing := to_distance (f (line, 2));

			elsif kw = keyword_layer then -- layer 2
				expect_field_count (line, 2);
				signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
				validate_signal_layer;
				
			elsif kw = keyword_width then -- width 0.3
				expect_field_count (line, 2);
				polygon_width_min := to_distance (f (line, 2));

			elsif kw = keyword_pad_technology then -- pad_technology smt_only/tht_only/smt_and_tht
				expect_field_count (line, 2);
				relief_properties.technology := to_pad_technology (f (line, 2));

			elsif kw = keyword_connection then -- connection thermal/solid
				expect_field_count (line, 2);
				pad_connection := to_pad_connection (f (line, 2));
				
			elsif kw = keyword_relief_width_min then -- relief_width_min 0.3
				expect_field_count (line, 2);
				relief_properties.width_min := to_distance (f (line, 2));

			elsif kw = keyword_relief_gap_max then -- relief_gap_max 0.7
				expect_field_count (line, 2);
				relief_properties.gap_max := to_distance (f (line, 2));

			else
				invalid_keyword (kw);
			end if;
		end read_fill_zone_route;


		
		
		procedure read_fill_zone_non_conductor is
			use et_board_geometry.pac_geometry_2;
			use et_fill_zones;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_fill_style then -- fill_style solid/hatched
				expect_field_count (line, 2);													
				board_fill_style := to_fill_style (f (line, 2));
			
			else
				invalid_keyword (kw);
			end if;
		end read_fill_zone_non_conductor;



		
		procedure read_fill_zone_keepout is
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_filled then -- filled yes/no
				expect_field_count (line, 2);													
				board_filled := to_filled (f (line, 2));

			else
				invalid_keyword (kw);
			end if;
		end read_fill_zone_keepout;



		
		procedure read_fill_zone_restrict is
			use et_pcb_stack;
			use et_board_geometry.pac_geometry_2;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_filled then -- filled yes/no
				expect_field_count (line, 2);													
				board_filled := to_filled (f (line, 2));

			elsif kw = keyword_layers then -- layers 1 14 3

				-- there must be at least two fields:
				expect_field_count (line => line, count_expected => 2, warn => false);
				signal_layers := to_layers (line, check_layers);

			else
				invalid_keyword (kw);
			end if;
		end read_fill_zone_restrict;



		
		procedure read_fill_zone_conductor_non_electric is
			use et_pcb_stack;
			use et_fill_zones;
			use et_fill_zones.boards;
			use et_board_geometry.pac_geometry_2;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_fill_style then -- fill_style solid/hatched
				expect_field_count (line, 2);													
				board_fill_style := to_fill_style (f (line, 2));

			elsif kw = keyword_easing_style then -- easing_style none/chamfer/fillet
				expect_field_count (line, 2);													
				board_easing.style := to_easing_style (f (line, 2));

			elsif kw = keyword_easing_radius then -- easing_radius 0.4
				expect_field_count (line, 2);													
				board_easing.radius := to_distance (f (line, 2));
				
			elsif kw = keyword_spacing then -- spacing 0.3
				expect_field_count (line, 2);													
				fill_spacing := to_distance (f (line, 2));

			elsif kw = keyword_width then -- width 0.5
				expect_field_count (line, 2);
				polygon_width_min := to_distance (f (line, 2));
				
			elsif kw = keyword_layer then -- layer 1
				expect_field_count (line, 2);
				signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
				validate_signal_layer;
				
			elsif kw = keyword_priority then -- priority 2
				expect_field_count (line, 2);
				contour_priority := to_priority (f (line, 2));

			elsif kw = keyword_isolation then -- isolation 0.5
				expect_field_count (line, 2);
				polygon_isolation := to_distance (f (line, 2));
				
			else
				invalid_keyword (kw);
			end if;
		end read_fill_zone_conductor_non_electric;



		-- submodules	
		submodule_port_name	: pac_net_name.bounded_string; -- RESET
		submodule_ports		: et_submodules.pac_submodule_ports.map;
		submodule_name 		: et_module_instance.pac_module_instance_name.bounded_string; -- MOT_DRV_3
		submodule_port 		: et_submodules.type_submodule_port;
		submodule 			: et_submodules.type_submodule;


		
		-- Reads the parameters of a submodule:
		procedure read_submodule is
			use et_schematic_coordinates;
			use et_submodules;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_file then -- file $ET_TEMPLATES/motor_driver.mod
				expect_field_count (line, 2);
				submodule.file := et_submodules.to_submodule_path (f (line, 2));

			elsif kw = keyword_name then -- name stepper_driver
				expect_field_count (line, 2);
				submodule_name := to_instance_name (f (line, 2));

			elsif kw = keyword_position then -- position sheet 3 x 130 y 210
				expect_field_count (line, 7);

				-- extract position of submodule starting at field 2
				submodule.position := to_position (line, 2);

			elsif kw = keyword_size then -- size x 30 y 30
				expect_field_count (line, 5);

				-- extract size of submodule starting at field 2
				submodule.size := to_size (line, 2);

			elsif kw = keyword_position_in_board then -- position_in_board x 23 y 0.2 rotation 90.0
				expect_field_count (line, 7);

				-- extract position of submodule starting at field 2
				submodule.position_in_board := et_board_geometry.pac_geometry_2.to_position (line, 2);

			elsif kw = keyword_view_mode then -- view_mode origin/instance
				expect_field_count (line, 2);
				submodule.view_mode := et_submodules.to_view_mode (f (line, 2));

			else
				invalid_keyword (kw);
			end if;
		end read_submodule;


		
		
		procedure read_submodule_port is
			use et_schematic_geometry.pac_geometry_2;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_name then -- name clk_out
				expect_field_count (line, 2);
				submodule_port_name := to_net_name (f (line, 2));

			elsif kw = keyword_position then -- position x 0 y 10
				expect_field_count (line, 5);

				-- extract port position starting at field 2
				submodule_port.position := to_vector_model (line, 2);

			elsif kw = keyword_direction then -- direction master/slave
				expect_field_count (line, 2);

				submodule_port.direction := et_submodules.to_port_name (f (line, 2));
			else
				invalid_keyword (kw);
			end if;
		end read_submodule_port;



		
		procedure insert_submodule_port is
			cursor : et_submodules.pac_submodule_ports.cursor;
			inserted : boolean;

			use et_schematic_ops.submodules;
			use pac_net_name;
		begin
			-- Test whether the port sits at the edge of the submodule box:
			if et_submodules.at_edge (submodule_port.position, submodule.size) then
				
				-- append port to collection of submodule ports
				et_submodules.pac_submodule_ports.insert (
					container	=> submodule_ports,
					key			=> submodule_port_name, -- RESET
					new_item	=> submodule_port,
					inserted	=> inserted,
					position	=> cursor
					);

				if not inserted then
					log (ERROR, "port " & 
						to_string (submodule_port_name) & " already used !",
						console => true
						);
					raise constraint_error;
				end if;

			else
				port_not_at_edge (submodule_port_name);
			end if;

			-- clean up for next port
			submodule_port_name := to_net_name ("");
			submodule_port := (others => <>);
			
		end insert_submodule_port;


		


		
		
		-- This variable is used for vector texts in conductor layers
		-- and restrict layers:
		board_text_conductor : et_conductor_text.boards.type_conductor_text;

		-- This variable is used for text placeholders in conductor layers:
		board_text_conductor_placeholder : et_pcb_placeholders.type_text_placeholder_conductors;


		
		procedure read_board_text_conductor_placeholder is
			use et_board_geometry.pac_geometry_2;
			use et_pcb_stack;
			use et_pcb_placeholders;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
				expect_field_count (line, 7);

				-- extract position of note starting at field 2
				board_text_conductor_placeholder.position := to_position (line, 2);

			elsif kw = keyword_size then -- size 1.000
				expect_field_count (line, 2);
				board_text_conductor_placeholder.size := to_distance (f (line, 2));

			elsif kw = keyword_linewidth then -- linewidth 0.1
				expect_field_count (line, 2);
				board_text_conductor_placeholder.line_width := to_distance (f (line, 2));

			elsif kw = keyword_alignment then -- alignment horizontal center vertical center
				expect_field_count (line, 5);

				-- extract alignment starting at field 2
				board_text_conductor_placeholder.alignment := to_alignment (line, 2);
				
			elsif kw = keyword_meaning then -- meaning revision/project_name/...
				expect_field_count (line, 2);
				board_text_conductor_placeholder.meaning := to_meaning (f (line, 2));

			elsif kw = keyword_layer then -- layer 15
				expect_field_count (line, 2);
				board_text_conductor_placeholder.layer := et_pcb_stack.to_signal_layer (f (line, 2));
				validate_signal_layer (board_text_conductor_placeholder.layer);
				
			else
				invalid_keyword (kw);
			end if;
		end read_board_text_conductor_placeholder;



		
		procedure read_schematic_text is
			use et_schematic_text;
			use et_schematic_geometry;
			use et_schematic_coordinates;	
			use pac_geometry_2;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position sheet 2 x 91.44 y 118.56
				expect_field_count (line, 7);

				declare
					-- extract position of schematic_text starting at field 2
					pos : constant type_object_position := 
						to_position (line, 2);
				begin
					schematic_text.position := pos.place;
					schematic_text.sheet := get_sheet (pos);
				end;

			elsif kw = keyword_content then -- content "DUMMY TEXT IN CORE MODULE"
				expect_field_count (line, 2); -- actual content in quotes !
				schematic_text.content := et_text.to_content (f (line, 2));

			elsif kw = keyword_size then -- size 1.4
				expect_field_count (line, 2);
				schematic_text.size := to_distance (f (line, 2));

			elsif kw = keyword_rotation then -- rotation 90
				expect_field_count (line, 2);
				schematic_text.rotation := pac_text_schematic.to_rotation_doc (f (line, 2));

	-- 			elsif kw = keyword_style then -- style normal/italic
	-- 				expect_field_count (line, 2);
				-- schematic_text.font := et_symbol_model.to_text_style (f (line, 2)); -- CS
				-- CS: currently font and style are ignored.

			elsif kw = keyword_alignment then -- alignment horizontal center vertical center
				expect_field_count (line, 5);

				-- extract alignment starting at field 2
				schematic_text.alignment := to_alignment (line, 2);
				
			else
				invalid_keyword (kw);
			end if;
		end read_schematic_text;

		

		
		procedure read_board_text_non_conductor is 
			use et_board_geometry.pac_geometry_2;
			kw : constant  string := f (line, 1);
		begin
			case stack.parent (degree => 2) is
				when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STOPMASK 
					| SEC_KEEPOUT | SEC_STENCIL =>

					-- CS: In the following: set a corresponding parameter-found-flag
					if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
						expect_field_count (line, 7);

						-- extract position starting at field 2
						board_text.position := to_position (line, 2);

					elsif kw = keyword_size then -- size 1.000
						expect_field_count (line, 2);
						board_text.size := to_distance (f (line, 2));

					elsif kw = keyword_linewidth then -- linewidth 0.1
						expect_field_count (line, 2);
						board_text.line_width := to_distance (f (line, 2));

						-- CS validate against dru settings
						
					elsif kw = keyword_alignment then -- alignment horizontal center vertical center
						expect_field_count (line, 5);

						-- extract alignment starting at field 2
						board_text.alignment := to_alignment (line, 2);
						
					elsif kw = keyword_content then -- content "WATER KETTLE CONTROL"
						expect_field_count (line, 2); -- actual content in quotes !
						board_text.content := et_text.to_content (f (line, 2));
						
					else
						invalid_keyword (kw);
					end if;
					
				when others => invalid_section;
			end case;
		end read_board_text_non_conductor;



		
		procedure read_board_text_conductor is
			use et_board_geometry.pac_geometry_2;
			use et_pcb_stack;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
				expect_field_count (line, 7);

				-- extract position starting at field 2
				board_text_conductor.position := to_position (line, 2);

			elsif kw = keyword_size then -- size 1.000
				expect_field_count (line, 2);
				board_text_conductor.size := to_distance (f (line, 2));

			elsif kw = keyword_linewidth then -- linewidth 0.1
				expect_field_count (line, 2);
				board_text_conductor.line_width := to_distance (f (line, 2));

				-- CS validate against dru settings
				
			elsif kw = keyword_alignment then -- alignment horizontal center vertical center
				expect_field_count (line, 5);

				-- extract alignment starting at field 2
				board_text_conductor.alignment := to_alignment (line, 2);
				
			elsif kw = keyword_content then -- content "TOP", "L2", "BOT"
				expect_field_count (line, 2); -- actual content in quotes !
				board_text_conductor.content := et_text.to_content (f (line, 2));

			elsif kw = keyword_layer then -- layer 15
				expect_field_count (line, 2);
				board_text_conductor.layer := et_pcb_stack.to_signal_layer (f (line, 2));
				validate_signal_layer (board_text_conductor.layer);
				
			else
				invalid_keyword (kw);
			end if;
		end read_board_text_conductor;

		

		
		procedure read_board_text_contours is 
			use et_board_geometry.pac_geometry_2;
			kw : constant  string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
				expect_field_count (line, 7);

				-- extract position starting at field 2
				board_text.position := to_position (line, 2);

			elsif kw = keyword_size then -- size 1.000
				expect_field_count (line, 2);
				board_text.size := to_distance (f (line, 2));

			elsif kw = keyword_linewidth then -- linewidth 0.1
				expect_field_count (line, 2);
				board_text.line_width := to_distance (f (line, 2));

			elsif kw = keyword_alignment then -- alignment horizontal center vertical center
				expect_field_count (line, 5);

				-- extract alignment starting at field 2
				board_text.alignment := to_alignment (line, 2);
				
			elsif kw = keyword_content then -- content "WATER KETTLE CONTROL"
				expect_field_count (line, 2); -- actual content in quotes !
				board_text.content := et_text.to_content (f (line, 2));
				
			else
				invalid_keyword (kw);
			end if;
		end read_board_text_contours;


		
		
		procedure read_layer is
			kw : constant string := f (line, 1);
			use et_pcb_stack;
			use package_layers;
			use et_board_geometry.pac_geometry_2;
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_conductor then -- conductor 1 0.035
				expect_field_count (line, 3);
				conductor_layer := to_signal_layer (f (line, 2));
				conductor_thickness := to_distance (f (line, 3));
				board_layer.conductor.thickness := conductor_thickness;

				-- Layer numbers must be continuous from top to bottom.
				-- After the dielectric of a layer the next conductor layer must
				-- have the next number:
				if dielectric_found then
					if to_index (board_layers.last) /= conductor_layer - 1 then
						log (ERROR, "expect conductor layer number" &
							to_string (to_index (board_layers.last) + 1) & " !",
							console => true);
						raise constraint_error;
					end if;
				end if;
				
				dielectric_found := false;

			elsif kw = keyword_dielectric then -- dielectric 1 1.5
				expect_field_count (line, 3);
				dielectric_layer := to_signal_layer (f (line, 2));
				board_layer.dielectric.thickness := to_distance (f (line, 3));
				dielectric_found := true;
				
				if dielectric_layer = conductor_layer then
					append (board_layers, board_layer);
				else
					log (ERROR, "expect dielectric layer number" & to_string (conductor_layer) & " !", console => true);
					raise constraint_error;
				end if;
			else
				invalid_keyword (kw);
			end if;
		end;



		
		
		
		-- This variable provides the basic things for a simple drill
		-- and a via (the type_via is derived from type_drill):
		drill : et_drills.type_drill;

		
		-- Via properties:
		via_category : et_vias.type_via_category;
		via_restring_inner : type_restring_width; -- CS default DRC
		via_restring_outer : type_restring_width; -- CS default DRC	
		via_layers_buried : et_vias.type_buried_layers;
		via_layer_blind : et_vias.type_via_layer;


		
		procedure read_via is
			use et_board_geometry.pac_geometry_2;
			use et_pcb;
			use et_vias;
			use et_terminals;
			use et_pcb_stack;
			use et_board_ops;
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 22.3 y 23.3
				expect_field_count (line, 5);

				-- extract the position starting at field 2 of line
				drill.position := to_vector_model (line, 2);

			elsif kw = keyword_via_category then -- category through/buried/...
				expect_field_count (line, 2);
				via_category := to_via_category (f (line, 2));
				
			elsif kw = keyword_diameter then -- diameter 0.35
				expect_field_count (line, 2);
				drill.diameter := to_distance (f (line, 2));
				-- CS validate against dru settings
							
			elsif kw = keyword_restring_outer then -- restring_outer 0.3
				expect_field_count (line, 2);
				via_restring_outer := to_distance (f (line, 2));
				-- CS validate against dru settings
				
			elsif kw = keyword_restring_inner then -- restring_inner 0.34
				expect_field_count (line, 2);
				via_restring_inner := to_distance (f (line, 2));
				-- CS validate against dru settings
							
			elsif kw = keyword_layers then -- layers 2 3 (for buried via only)
				expect_field_count (line, 3);
				via_layers_buried := to_buried_layers (
							upper	=> f (line, 2),
							lower	=> f (line, 3),
							bottom	=> get_deepest_conductor_layer (module_cursor));
				
			elsif kw = keyword_destination then -- destination 15 (for blind via only)
				expect_field_count (line, 2);
				via_layer_blind := et_pcb_stack.to_signal_layer (f (line, 2));
				-- CS exception rises if layer out of range (i.e. less than 2).
				--validate_signal_layer (via_layers_buried.lower);
				
			else
				invalid_keyword (kw);
			end if;
			
		end read_via;



		
		procedure build_via is 
			use et_vias;
			use pac_vias;
		begin
			-- insert via in route.vias
			case via_category is
				when THROUGH =>
					append (route.vias, ((drill with
						category		=> THROUGH,
						restring_inner	=> via_restring_inner,
						restring_outer	=> via_restring_outer)));

				when BLIND_DRILLED_FROM_TOP =>
					-- CS validate via_layer_blind. must be higher than 
					-- deepest used layer.
					
					append (route.vias, ((drill with
						category		=> BLIND_DRILLED_FROM_TOP,
						restring_inner	=> via_restring_inner,
						restring_top	=> via_restring_outer,
						lower			=> via_layer_blind)));

				when BLIND_DRILLED_FROM_BOTTOM =>
					-- CS validate via_layer_blind. must be lower than 
					-- top layer and higher than deepest used layer.
					
					append (route.vias, ((drill with
						category		=> BLIND_DRILLED_FROM_BOTTOM,
						restring_inner	=> via_restring_inner,
						restring_bottom	=> via_restring_outer,
						upper			=> via_layer_blind)));

				when BURIED =>
					-- CS validate via_layers_buried. must be higher than 
					-- deepst used layer.
					
					append (route.vias, ((drill with
						category		=> BURIED,
						restring_inner	=> via_restring_inner,
						layers			=> via_layers_buried)));
					
			end case;

			drill := (others => <>); -- clean up for next via
			via_category := via_category_default;
			via_layers_buried := (others => <>);
			via_layer_blind := type_via_layer'first;
			-- CS
			-- via_restring_inner := DRC ?
			-- via_restring_outer := 
		end build_via;

		
		-- temporarily storage place for user settings
		user_settings_board : et_pcb.type_user_settings;


		
		procedure read_user_settings_vias is
			use et_board_geometry.pac_geometry_2;
			kw : constant string := f (line, 1);
		begin
			-- via drill
			if kw = keyword_via_drill then
				expect_field_count (line, 2);
				
				if f (line, 2) = keyword_dru then -- drill dru
					user_settings_board.vias.drill.active := false;
				else -- drill 0.6
					user_settings_board.vias.drill.active := true;
					user_settings_board.vias.drill.size := to_distance (f (line, 2));

					-- CS validate against dru settings
				end if;

			-- inner restring
			elsif kw = keyword_restring_inner then
				expect_field_count (line, 2);

				if f (line, 2) = keyword_dru then -- restring_inner dru
					user_settings_board.vias.restring_inner.active := false;
				else -- restring_inner 0.22
					user_settings_board.vias.restring_inner.active := true;
					user_settings_board.vias.restring_inner.width := to_distance (f (line, 2));
					
					-- CS validate against dru settings
				end if;

			-- outer restring
			elsif kw = keyword_restring_outer then
				expect_field_count (line, 2);

				if f (line, 2) = keyword_dru then -- restring_outer dru
					user_settings_board.vias.restring_outer.active := false;
				else -- restring_outer 0.2
					user_settings_board.vias.restring_outer.active := true;
					user_settings_board.vias.restring_outer.width := to_distance (f (line, 2));

					-- CS validate against dru settings
				end if;
				
			else
				invalid_keyword (kw);
			end if;
		end read_user_settings_vias;



		
		procedure read_user_settings_fill_zones_conductor is
			use et_fill_zones;
			use et_fill_zones.boards;		
			use et_thermal_relief;
			use et_board_geometry.pac_geometry_2;
			kw : constant string := f (line, 1);
		begin
			if kw = keyword_fill_style then -- fill_style solid/hatched
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.fill_style := to_fill_style (f (line, 2));

			elsif kw = keyword_linewidth then -- linewidth 0.3
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.linewidth := to_distance (f (line, 2));

			elsif kw = keyword_priority then -- priority 2
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.priority_level := to_priority (f (line, 2));

			elsif kw = keyword_isolation then -- isolation 0.4
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.isolation := to_distance (f (line, 2));

			elsif kw = keyword_spacing then -- spacing 0.5
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.spacing := to_distance (f (line, 2));

			elsif kw = keyword_connection then -- connection thermal/solid
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.connection := to_pad_connection (f (line, 2));

			elsif kw = keyword_pad_technology then -- pad_technology smt_and_tht
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.thermal.technology := to_pad_technology (f (line, 2));

			elsif kw = keyword_relief_width_min then -- relief_width_min 0.25
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.thermal.width_min := to_distance (f (line, 2));

			elsif kw = keyword_relief_gap_max then -- relief_gap_max 0.25
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.thermal.gap_max := to_distance (f (line, 2));

			elsif kw = keyword_easing_style then -- easing_style none/chamfer/fillet
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.easing.style := to_easing_style (f (line, 2));

			elsif kw = keyword_easing_radius then -- easing_radius 1.0
				expect_field_count (line, 2);
				user_settings_board.polygons_conductor.easing.radius := to_distance (f (line, 2));
				
			else
				invalid_keyword (kw);
			end if;

			-- CS plausibility check ?
		end read_user_settings_fill_zones_conductor;



		
		procedure assign_user_settings_board is
			procedure do_it (
				module_name	: in pac_module_name.bounded_string;
				module		: in out type_generic_module) 
			is begin
				module.board.user_settings := user_settings_board;
			end do_it;
		begin
			update_element (generic_modules, module_cursor, do_it'access);
		end assign_user_settings_board;


		
		
		procedure process_line is 
			
			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
			

				procedure add_board_layer is 
					use et_board_ops;
					

					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is
						use et_pcb_stack;
					begin
						log (text => "board layer stack", level => log_threshold + 1);

						-- Copy the collected layers (except the bottom conductor layer) into the module:
						module.board.stack.layers := board_layers;

						-- If the last entry was "conductor n t" then we assume that this
						-- was the bottom conductor layer (it does not have a dielectric layer underneath).
						if not dielectric_found then
							module.board.stack.bottom.thickness := conductor_thickness;
						else
							log (ERROR, "dielectric not allowed underneath the bottom conductor layer !", console => true);
							raise constraint_error;
						end if;
						
						-- reset layer values:
						dielectric_found := false;
						conductor_layer := et_pcb_stack.type_signal_layer'first;
						dielectric_layer := et_pcb_stack.type_signal_layer'first;
						conductor_thickness := et_pcb_stack.conductor_thickness_outer_default;
						board_layer := (others => <>);
						package_layers.clear (board_layers);

					end do_it;


				begin							 
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- Now that the board layer stack is complete,
					-- we assign the deepest layer to check_layers.
					check_layers.deepest_layer := 
						get_deepest_conductor_layer (module_cursor);

				end add_board_layer;
				
				



				
				procedure insert_submodule (
					module_name	: in pac_module_name.bounded_string;
					module		: in out type_generic_module) 
				is
					inserted : boolean;
					use et_submodules;
					use et_submodules.pac_submodules;
					cursor : et_submodules.pac_submodules.cursor;
				begin
					log (text => "submodule " & to_string (submodule_name), level => log_threshold + 1);

					-- CS: notify about missing parameters (by reading the parameter-found-flags)
					-- If a parameter is missing, the default is assumed. See type_submodule spec.
					
					pac_submodules.insert (
						container	=> module.submods,
						key			=> submodule_name,	-- the instance name like MOT_DRV_3
						new_item	=> submodule,
						inserted	=> inserted,
						position	=> cursor);

					if not inserted then
						log (ERROR, "submodule '" & to_string (submodule_name) 
							& "' already exists !", console => true);
						raise constraint_error;
					end if;

					-- The submodule/template (kept in submodule.file) will be read later once the 
					-- parent module has been read completely.
					
					-- clean up for next submodule
					submodule_name := to_instance_name ("");
					submodule := (others => <>);
					
				end insert_submodule;

				

				

				procedure set_frame_schematic is

					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is
						use et_drawing_frame;
						use et_drawing_frame.schematic;
						use et_drawing_frame_rw;
					begin
						log (text => "drawing frame schematic " & to_string (frame_template_schematic), 
							level => log_threshold + 1);

						-- set the frame template name
						module.frames.template := frame_template_schematic;

						-- assign the sheet descriptions:
						module.frames.descriptions := sheet_descriptions;

						-- Clean up sheet descriptions even if
						-- there should not be another section for sheet descriptions:
						pac_schematic_descriptions.clear (sheet_descriptions);
						
						-- read the frame template file
						module.frames.frame := read_frame_schematic (
							file_name		=> frame_template_schematic,
							log_threshold	=> log_threshold + 2);

					end do_it;
					

				begin
					-- set schematic frame template
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);
					
				end set_frame_schematic;

				


				
				procedure add_sheet_description is 
					use et_schematic_coordinates;	
					use et_drawing_frame.schematic;
					use et_sheets;
					use pac_schematic_descriptions;
					inserted : boolean;
					position : pac_schematic_descriptions.cursor;
				begin
					insert (
						container	=> sheet_descriptions,
						key			=> sheet_description_number,
						inserted	=> inserted,
						position	=> position,
						new_item	=> (sheet_description_text, sheet_description_category)
						);

					-- clean up for next sheet description
					sheet_description_category := schematic_sheet_category_default;
					sheet_description_number := type_sheet'first;
					sheet_description_text := to_content("");
				end add_sheet_description;

		
				

				procedure set_frame_board is

					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is
						use et_drawing_frame;
						use et_drawing_frame_rw;
					begin
						log (text => "drawing frame board " & to_string (frame_template_board), level => log_threshold + 1);

						-- set the frame template name
						module.board.frame.template := frame_template_board;

						-- read the frame template file
						module.board.frame.frame := read_frame_board (
							file_name		=> frame_template_board,
							log_threshold	=> log_threshold + 2);

						-- Set the frame position:
						module.board.frame.frame.position := frame_board_position;
					end do_it;

				begin
					-- set board/layout frame template
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

				end set_frame_board;


				
				
				procedure insert_schematic_text (
					module_name	: in pac_module_name.bounded_string;
					module		: in out type_generic_module) 
				is begin
					-- append schematic note to collection of notes
					et_schematic_text.pac_texts.append (module.texts, schematic_text);

					-- clean up for next note
					schematic_text := (others => <>);
				end insert_schematic_text;


				


				use et_board_layer_category;
				

				procedure insert_line (
					layer_cat	: in type_layer_category;
					face		: in et_pcb_sides.type_face) -- TOP, BOTTOM
				is
				-- The board_line and its board_line_width have been general things until now.
				-- Depending on the layer and the side of the board (face) the board_line
				-- is now assigned to the board where it belongs to.

					use et_board_geometry;
					use pac_geometry_2;
					
					use et_stopmask;
					use et_stencil;
					use et_silkscreen;
					use et_assy_doc;

					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module)
					is 					
						use et_pcb_sides;
					begin
						case face is
							when TOP =>
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										pac_silk_lines.append (
											container	=> module.board.silkscreen.top.lines,
											new_item	=> (type_line (board_line) with board_line_width));

									when LAYER_CAT_ASSY =>
										pac_doc_lines.append (
											container	=> module.board.assy_doc.top.lines,
											new_item	=> (type_line (board_line) with board_line_width));

									when LAYER_CAT_STENCIL =>
										pac_stencil_lines.append (
											container	=> module.board.stencil.top.lines,
											new_item	=> (type_line (board_line) with board_line_width));
										
									when LAYER_CAT_STOPMASK =>
										pac_stop_lines.append (
											container	=> module.board.stopmask.top.lines,
											new_item	=> (type_line (board_line) with board_line_width));

									when others => null; -- CS raise exception ?								
								end case;
								
							when BOTTOM => null;
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										pac_silk_lines.append (
											container	=> module.board.silkscreen.bottom.lines,
											new_item	=> (type_line (board_line) with board_line_width));

									when LAYER_CAT_ASSY =>
										pac_doc_lines.append (
											container	=> module.board.assy_doc.bottom.lines,
											new_item	=> (type_line (board_line) with board_line_width));
										
									when LAYER_CAT_STENCIL =>
										pac_stencil_lines.append (
											container	=> module.board.stencil.bottom.lines,
											new_item	=> (type_line (board_line) with board_line_width));
										
									when LAYER_CAT_STOPMASK =>
										pac_stop_lines.append (
											container	=> module.board.stopmask.bottom.lines,
											new_item	=> (type_line (board_line) with board_line_width));

									when others => null; -- CS raise exception ?
								end case;
								
						end case;
					end do_it;

					
				begin -- insert_line
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_line;
					board_reset_line_width;
				end insert_line;


				
				
				procedure insert_arc (
					layer_cat	: in type_layer_category;
					face		: in et_pcb_sides.type_face) -- TOP, BOTTOM
				is
				-- The board_arc and its board_line_width have been general things until now. 
				-- Depending on the layer and the side of the board (face) the board_arc
				-- is now assigned to the board where it belongs to.

					use et_board_geometry;
					use pac_geometry_2;
					
					use et_stopmask;
					use et_stencil;
					use et_silkscreen;
					use et_assy_doc;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is 
						use et_pcb_sides;
					begin
						case face is
							when TOP =>
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										pac_silk_arcs.append (
											container	=> module.board.silkscreen.top.arcs,
											new_item	=> (type_arc (board_arc) with board_line_width));

									when LAYER_CAT_ASSY =>
										pac_doc_arcs.append (
											container	=> module.board.assy_doc.top.arcs,
											new_item	=> (type_arc (board_arc) with board_line_width));

									when LAYER_CAT_STENCIL =>
										pac_stencil_arcs.append (
											container	=> module.board.stencil.top.arcs,
											new_item	=> (type_arc (board_arc) with board_line_width));
										
									when LAYER_CAT_STOPMASK =>
										pac_stop_arcs.append (
											container	=> module.board.stopmask.top.arcs,
											new_item	=> (type_arc (board_arc) with board_line_width));

									when others => null;  -- CS raise exception ?
								end case;

								
							when BOTTOM => null;
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										pac_silk_arcs.append (
											container	=> module.board.silkscreen.bottom.arcs,
											new_item	=> (type_arc (board_arc) with board_line_width));

									when LAYER_CAT_ASSY =>
										pac_doc_arcs.append (
											container	=> module.board.assy_doc.bottom.arcs,
											new_item	=> (type_arc (board_arc) with board_line_width));
										
									when LAYER_CAT_STENCIL =>
										pac_stencil_arcs.append (
											container	=> module.board.stencil.bottom.arcs,
											new_item	=> (type_arc (board_arc) with board_line_width));
										
									when LAYER_CAT_STOPMASK =>
										pac_stop_arcs.append (
											container	=> module.board.stopmask.bottom.arcs,
											new_item	=> (type_arc (board_arc) with board_line_width));

									when others => null;  -- CS raise exception ?
								end case;
								
						end case;
					end do_it;

					
				begin -- insert_arc
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board arc
					board_reset_arc;
					board_reset_line_width;
				end insert_arc;



				
				
				procedure insert_circle (
					layer_cat	: in type_layer_category;
					face		: in et_pcb_sides.type_face) -- TOP, BOTTOM
				is
				-- The board_circle has been a general thing until now. 
				-- Depending on the layer and the side of the board (face) the board_circle
				-- is now assigned to the board where it belongs to.

					use et_board_geometry;
					use pac_geometry_2;
					
					use et_stopmask;
					use et_stencil;
					use et_silkscreen;
					use et_assy_doc;

					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is
						use et_pcb_sides;
						use et_board_coordinates;
					begin
						case face is
							when TOP =>
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										pac_silk_circles.append (
											container	=> module.board.silkscreen.top.circles,
											new_item	=> (type_circle (board_circle) with board_line_width));

									when LAYER_CAT_ASSY =>
										pac_doc_circles.append (
											container	=> module.board.assy_doc.top.circles,
											new_item	=> (type_circle (board_circle) with board_line_width));

									when LAYER_CAT_STENCIL =>
										pac_stencil_circles.append (
											container	=> module.board.stencil.top.circles,
											new_item	=> (type_circle (board_circle) with board_line_width));
										
									when LAYER_CAT_STOPMASK =>
										pac_stop_circles.append (
											container	=> module.board.stopmask.top.circles,
											new_item	=> (type_circle (board_circle) with board_line_width));

									when others => null;  -- CS raise exception ?
								end case;
								
							when BOTTOM =>
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										pac_silk_circles.append (
											container	=> module.board.silkscreen.bottom.circles,
											new_item	=> (type_circle (board_circle) with board_line_width));

									when LAYER_CAT_ASSY =>
										pac_doc_circles.append (
											container	=> module.board.assy_doc.bottom.circles,
											new_item	=> (type_circle (board_circle) with board_line_width));
										
									when LAYER_CAT_STENCIL =>
										pac_stencil_circles.append (
											container	=> module.board.stencil.bottom.circles,
											new_item	=> (type_circle (board_circle) with board_line_width));
										
									when LAYER_CAT_STOPMASK =>
										pac_stop_circles.append (
											container	=> module.board.stopmask.bottom.circles,
											new_item	=> (type_circle (board_circle) with board_line_width));

									when others => null;  -- CS raise exception ?
								end case;
								
						end case;
					end do_it;

					
				begin -- insert_circle
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board circle
					board_reset_line_width;
					board_reset_circle;
				end insert_circle;


				
				
				procedure insert_polygon ( -- CS rename to insert_contour
					layer_cat	: in type_layer_category;
					face		: in et_pcb_sides.type_face) -- TOP, BOTTOM
				is
				-- The polygon has been a general thing until now. 
				-- Depending on the layer and the side of the board (face) the polygon
				-- is now assigned to the board where it belongs to.

					use et_board_geometry;
					use pac_geometry_2;
					
					use et_stopmask;
					use et_stencil;
					use et_silkscreen;
					use et_assy_doc;
					use et_keepout;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module)
					is
						use pac_contours;
						use et_pcb_sides;
						
						procedure append_silk_polygon_top is begin
							pac_silk_zones.append (
								container	=> module.board.silkscreen.top.zones,
								new_item	=> (contour with null record));
						end;

						
						procedure append_silk_polygon_bottom is begin
							pac_silk_zones.append (
								container	=> module.board.silkscreen.bottom.zones,
								new_item	=> (contour with null record));
						end;

						
						procedure append_assy_doc_polygon_top is begin
							pac_doc_zones.append (
								container	=> module.board.assy_doc.top.zones,
								new_item	=> (contour with null record));
						end;

						
						procedure append_assy_doc_polygon_bottom is begin
							pac_doc_zones.append (
								container	=> module.board.assy_doc.bottom.zones,
								new_item	=> (contour with null record));
						end;

						
						procedure append_keepout_polygon_top is begin
							pac_keepout_zones.append (
								container	=> module.board.keepout.top.zones, 
								new_item	=> (contour with null record));
						end;

						
						procedure append_keepout_polygon_bottom is begin
							pac_keepout_zones.append (
								container	=> module.board.keepout.bottom.zones, 
								new_item	=> (contour with null record));
						end;

						
						procedure append_stencil_polygon_top is begin
							pac_stencil_zones.append (
								container	=> module.board.stencil.top.zones,
								new_item	=> (contour with null record));
						end;

						
						procedure append_stencil_polygon_bottom is begin
							pac_stencil_zones.append (
								container	=> module.board.stencil.bottom.zones,
								new_item	=> (contour with null record));
						end;

						
						procedure append_stop_polygon_top is begin
							pac_stop_zones.append (
								container	=> module.board.stopmask.top.zones,
								new_item	=> (contour with null record));
						end;

						
						procedure append_stop_polygon_bottom is begin
							pac_stop_zones.append (
								container	=> module.board.stopmask.bottom.zones,
								new_item	=> (contour with null record));
						end;

						
					begin -- do_it
						case face is
							when TOP =>
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										append_silk_polygon_top;
													
									when LAYER_CAT_ASSY =>
										append_assy_doc_polygon_top;

									when LAYER_CAT_STENCIL =>
										append_stencil_polygon_top;
										
									when LAYER_CAT_STOPMASK =>
										append_stop_polygon_top;
										
									when LAYER_CAT_KEEPOUT =>
										append_keepout_polygon_top;

									when others => null; -- CS raise exception ?
								end case;
								
							when BOTTOM =>
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										append_silk_polygon_bottom;

									when LAYER_CAT_ASSY =>
										append_assy_doc_polygon_bottom;
										
									when LAYER_CAT_STENCIL =>
										append_stencil_polygon_bottom;
										
									when LAYER_CAT_STOPMASK =>
										append_stop_polygon_bottom;
										
									when LAYER_CAT_KEEPOUT =>
										append_keepout_polygon_bottom;

									when others => null; -- CS raise exception ?
								end case;
								
						end case;
					end do_it;

					
				begin
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board polygon
					board_reset_contour;
				end insert_polygon;

				
				
				procedure insert_cutout (
					layer_cat	: in type_layer_category; -- CS no need anymore ?
					face		: in et_pcb_sides.type_face) -- TOP, BOTTOM
				is
				-- The polygon has been a general thing until now. 
				-- Depending on the layer category and the side of the board (face) the polygon
				-- is threated as a cutout zone and assigned to the board where it belongs to.

					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module)
					is
						use et_pcb_sides;
						use et_board_geometry;
						use pac_contours;
						use et_stopmask;
						use et_keepout;
						
						
						procedure append_keepout_cutout_top is begin
							pac_keepout_cutouts.append (
								container	=> module.board.keepout.top.cutouts, 
								new_item	=> (contour with null record));
						end;

						procedure append_keepout_cutout_bottom is begin
							pac_keepout_cutouts.append (
								container	=> module.board.keepout.bottom.cutouts, 
								new_item	=> (contour with null record));
						end;

						
					begin -- do_it
						case face is
							when TOP =>
								case layer_cat is
									when LAYER_CAT_KEEPOUT =>
										append_keepout_cutout_top;

									when others => null; -- CS raise exception ?
								end case;
								
							when BOTTOM => null;
								case layer_cat is
									when LAYER_CAT_KEEPOUT =>
										append_keepout_cutout_bottom;

									when others => null; -- CS raise exception ?									
								end case;							
						end case;
					end do_it;

					
				begin -- insert_cutout
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board cutout
					board_reset_contour;
				end insert_cutout;



				
				procedure insert_cutout_via_restrict is
					use et_board_geometry;
					use pac_contours;				
					use et_via_restrict.boards;
					use et_pcb_stack;
					use type_signal_layers;

					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) is
					begin
						pac_via_restrict_cutouts.append (
							container	=> module.board.via_restrict.cutouts,
							new_item	=> (contour with
											layers	=> signal_layers));
					end do_it;

					
				begin
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board contour
					board_reset_contour;

					clear (signal_layers);
				end insert_cutout_via_restrict;

				
				
				procedure insert_cutout_route_restrict is
					use et_board_geometry;
					use pac_contours;
					use et_route_restrict.boards;
					use et_pcb_stack;
					use type_signal_layers;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) is
					begin
						pac_route_restrict_cutouts.append (
							container	=> module.board.route_restrict.cutouts,
							new_item	=> (contour with 
											layers	=> signal_layers));
					end do_it;
										
				begin
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board contour
					board_reset_contour;

					clear (signal_layers);
				end insert_cutout_route_restrict;



				
				-- This is about cutout zones to trim floating contours in 
				-- signal layers. No connection to any net.
				procedure insert_cutout_conductor is
					use et_board_geometry;
					use pac_contours;
					use et_pcb;
					use et_fill_zones.boards;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) is
					begin
						pac_cutouts.append (
							container	=> module.board.conductors_floating.cutouts,
							new_item	=> (contour with
									layer => signal_layer));
					end do_it;
										
				begin -- insert_cutout_conductor
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next floating board contour
					board_reset_contour;
				end insert_cutout_conductor;


					
				procedure insert_placeholder (
					layer_cat	: in type_layer_category;
					face		: in et_pcb_sides.type_face)  -- TOP, BOTTOM
				is
				-- The board_text_placeholder has been a general thing until now. 
				-- Depending on the layer and the side of the board (face) the board_text_placeholder
				-- is now assigned to the board where it belongs to.
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is
						use et_pcb_sides;
						use et_board_coordinates;
						use et_pcb;
						use et_board_text;
						use et_pcb_placeholders;
					begin
						case face is
							when TOP =>
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										pac_text_placeholders.append (
											container	=> module.board.silkscreen.top.placeholders,
											new_item	=> board_text_placeholder);

									when LAYER_CAT_ASSY =>
										pac_text_placeholders.append (
											container	=> module.board.assy_doc.top.placeholders,
											new_item	=> board_text_placeholder);

									when LAYER_CAT_STOPMASK =>
										pac_text_placeholders.append (
											container	=> module.board.stopmask.top.placeholders,
											new_item	=> board_text_placeholder);

									-- CS
									--when KEEPOUT =>
									--	pac_text_placeholders.append (
									--		container	=> module.board.keepout.top.placeholders,
									--		new_item	=> board_text_placeholder);

									when others => invalid_section;
								end case;
								
							when BOTTOM =>
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										pac_text_placeholders.append (
											container	=> module.board.silkscreen.bottom.placeholders,
											new_item	=> board_text_placeholder);

									when LAYER_CAT_ASSY =>
										pac_text_placeholders.append (
											container	=> module.board.assy_doc.bottom.placeholders,
											new_item	=> board_text_placeholder);
										
									when LAYER_CAT_STOPMASK =>
										pac_text_placeholders.append (
											container	=> module.board.stopmask.bottom.placeholders,
											new_item	=> board_text_placeholder);

									-- CS
									--when KEEPOUT =>
									--	pac_text_placeholders.append (
									--		container	=> module.board.keepout.bottom.placeholders,
									--		new_item	=> board_text_placeholder);

									when others => invalid_section;
								end case;
								
						end case;
					end do_it;

					
				begin -- insert_placeholder
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board placeholder
					board_text_placeholder := (others => <>);
				end insert_placeholder;


				
				procedure insert_line_route_restrict is
					use et_board_geometry;
					use pac_geometry_2;
					use et_route_restrict.boards;
					use et_pcb_stack;
					use type_signal_layers;

					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) is
					begin
						pac_route_restrict_lines.append (
							container	=> module.board.route_restrict.lines,
							new_item	=> (type_line (board_line) with 
											signal_layers));
					end do_it;

					
				begin -- insert_line_route_restrict
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_line;
					clear (signal_layers);
				end insert_line_route_restrict;


				
				procedure insert_arc_route_restrict is
					use et_board_geometry;
					use pac_geometry_2;
					use et_route_restrict.boards;
					use et_pcb_stack;					
					use type_signal_layers;

					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) is
					begin
						pac_route_restrict_arcs.append (
							container	=> module.board.route_restrict.arcs,
							new_item	=> (type_arc (board_arc) with
											signal_layers));
					end do_it;

					
				begin -- insert_arc_route_restrict
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_arc;

					clear (signal_layers);
				end insert_arc_route_restrict;

				
				procedure insert_circle_route_restrict is
					use et_board_geometry;
					use pac_geometry_2;
					use et_route_restrict.boards;
					use et_pcb_stack;
					use type_signal_layers;

					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) is
					begin
						pac_route_restrict_circles.append (
							container	=> module.board.route_restrict.circles,
							new_item	=> (type_circle (board_circle) with 
											signal_layers));
					end do_it;
										
				begin -- insert_circle_route_restrict
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board line
					board_reset_circle;
					clear (signal_layers);
				end insert_circle_route_restrict;

				
				
				procedure insert_polygon_route_restrict is
					use et_board_geometry;
					use pac_geometry_2;				
					use pac_contours;
					
					use et_route_restrict.boards;
					use pac_route_restrict_contours;
					
					use et_pcb_stack;
					use type_signal_layers;

					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is begin
						append (
							container	=> module.board.route_restrict.contours,
							new_item	=> (contour with signal_layers));
					end do_it;
										
				begin
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board polygon
					board_reset_contour;

					clear (signal_layers);
				end insert_polygon_route_restrict;


				
				procedure insert_zone_via_restrict is
					use et_board_geometry;
					use pac_geometry_2;
					use pac_contours;
					use et_via_restrict.boards;
					use pac_via_restrict_contours;
					use et_pcb_stack;
					use type_signal_layers;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is begin
						append (
							container	=> module.board.via_restrict.contours,
							new_item	=> (contour with signal_layers));
					end do_it;
										
				begin
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board contour
					board_reset_contour;

					clear (signal_layers);
				end insert_zone_via_restrict;


				
				-- This is about floating contours in signal layers. No connection to any net.
				procedure insert_polygon_conductor is
					use et_board_geometry;
					use pac_contours;
					use et_fill_zones;
					use et_fill_zones.boards;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is begin
						case board_fill_style is
							when SOLID =>
								pac_floating_solid.append (
									container	=> module.board.conductors_floating.zones.solid,
									new_item	=> (contour with
										fill_style 	=> SOLID,
										easing		=> board_easing,
										islands		=> no_islands,
										properties	=> (signal_layer, contour_priority, others => <>),
										isolation	=> polygon_isolation,
										linewidth	=> polygon_width_min));

							when HATCHED =>
								pac_floating_hatched.append (
									container	=> module.board.conductors_floating.zones.hatched,
									new_item	=> (contour with
										fill_style 	=> HATCHED,
										easing		=> board_easing,
										islands		=> no_islands,
										properties	=> (signal_layer, contour_priority, others => <>),
										isolation	=> polygon_isolation,
										linewidth	=> polygon_width_min,
										spacing		=> fill_spacing));
						end case;
					end do_it;

					
				begin -- insert_polygon_conductor
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next floating board polygon
					board_reset_contour;
				end insert_polygon_conductor;


				
				
				procedure insert_line_track is -- about freetracks
					use et_conductor_segment.boards;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is begin
						pac_conductor_lines.append (
							container	=> module.board.conductors_floating.lines,
							new_item	=> (et_board_geometry.pac_geometry_2.type_line (board_line) with
											width	=> board_line_width,
											layer	=> signal_layer));
					end;
										
				begin -- insert_line_track
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next track line
					board_reset_line;
					board_reset_line_width;
					board_reset_signal_layer;
				end insert_line_track;

				

				
				procedure insert_arc_track is -- about freetracks
					use et_conductor_segment.boards;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is begin
						pac_conductor_arcs.append (
							container	=> module.board.conductors_floating.arcs,
							new_item	=> (et_board_geometry.pac_geometry_2.type_arc (board_arc) with
											width	=> board_line_width,
											layer	=> signal_layer));
					end;

					
				begin -- insert_arc_track
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next track arc
					board_reset_arc;
					board_reset_line_width;
					board_reset_signal_layer;
				end insert_arc_track;



				
				procedure insert_circle_track is -- about freetracks
					use et_conductor_segment.boards;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) is
					begin
						pac_conductor_circles.append (
							container	=> module.board.conductors_floating.circles,
							--new_item	=> (board_make_conductor_circle with signal_layer));
							new_item	=> (et_board_geometry.pac_geometry_2.type_circle (board_circle) with 
											width	=> board_line_width, 
											layer	=> signal_layer));
					end;
										
				begin -- insert_circle_track
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next track circle
					board_reset_circle;
					board_reset_line_width;
					board_reset_signal_layer;
					-- CS reset other properties
				end insert_circle_track;


				
				
				procedure build_conductor_text is
					use et_board_text;
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is
						use et_board_geometry;
						use pac_geometry_2;
						
						use et_pcb;
						use et_conductor_text.boards;
						use pac_conductor_texts;
						use et_board_ops;

						mirror : type_mirror;
						
					begin
						mirror := signal_layer_to_mirror (board_text_conductor.layer, get_deepest_conductor_layer (module_cursor));

						-- vectorize the text:
						board_text_conductor.vectors := vectorize_text (
							content			=> board_text_conductor.content,
							size			=> board_text_conductor.size,
							rotation		=> get_rotation (board_text_conductor.position),
							position		=> board_text_conductor.position.place,
							mirror			=> mirror,
							line_width		=> board_text_conductor.line_width,
							make_border		=> true,
							log_threshold	=> log_threshold + 2
							-- CS alignment
							); 

						append (
							container	=> module.board.conductors_floating.texts,
							new_item	=> board_text_conductor);

					end do_it;

					
				begin -- build_conductor_text
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next text in conductor layer
					board_text_conductor := (others => <>);
				end build_conductor_text;

				
				
				procedure insert_board_text_placeholder is
					use et_pcb_placeholders;
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) is
					begin
						pac_text_placeholders_conductors.append (
							container	=> module.board.conductors_floating.placeholders,
							new_item	=> board_text_conductor_placeholder);
					end do_it;

					
				begin -- insert_board_text_placeholder
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next placeholder in conductor layer
					board_text_conductor_placeholder := (others => <>);
				end insert_board_text_placeholder;

				
				
				procedure insert_line_outline is
					use et_board_geometry;
					use pac_geometry_2;
					use pac_contours;
					use pac_segments;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is begin
						append (
							container	=> module.board.board_contour.outline.contour.segments,
							new_item	=> (pac_contours.LINE, board_line));
					end do_it;

					
				begin -- insert_line_outline
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next pcb contour line
					board_reset_line;
				end insert_line_outline;

				
				
				procedure insert_arc_outline is
					use et_board_geometry;
					use pac_geometry_2;
					use pac_contours;
					use pac_segments;

					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is begin
						append (
							container	=> module.board.board_contour.outline.contour.segments,
							new_item	=> (pac_contours.ARC, board_arc));
					end do_it;

					
				begin -- insert_arc_outline
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next pcb contour arc
					board_reset_arc;
				end insert_arc_outline;



				
				procedure insert_circle_outline is
					

					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is begin
						module.board.board_contour.outline.contour := (
							circular	=> true,
							circle		=> board_circle);
					end do_it;
										
				begin -- insert_circle_outline
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- Clean up for next pcb contour circle.
					-- NOTE: There should not be another circle for the outline,
					-- because only a single circle is allowed.
					board_reset_circle;
				end insert_circle_outline;


				
				-- holes in PCB (or cutouts)
				procedure append_hole is 
					use et_pcb_contour;
					use pac_holes;
					
					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out type_generic_module) 
					is begin
						append (
							container 	=> module.board.board_contour.holes,
							new_item	=> (contour with null record));
					end do_it;

				begin
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);
					
					-- clean up for next hole
					board_reset_contour;
				end append_hole;

				
				--procedure insert_text_contour is
					--use et_pcb;
					
					--procedure do_it (
						--module_name	: in pac_module_name.bounded_string;
						--module		: in out type_generic_module) is
					--begin
						--pac_pcb_contour_circles.append (
							--container	=> module.board.board_contour.texts,
							--new_item	=> (et_board_text.pac_geometry_2.type_circle (board_circle) with board_lock_status));
					--end do_it;
										
				--begin -- insert_text_contour
					--update_element (
						--container	=> generic_modules,
						--position	=> module_cursor,
						--process		=> do_it'access);

					---- clean up for next pcb contour circle
					--board_reset_circle;
					--board_reset_lock_status;
				--end insert_text_contour;

				
				procedure build_non_conductor_line (
					face : in et_pcb_sides.type_face)
				is
				begin
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_line (
								layer_cat	=> LAYER_CAT_SILKSCREEN,
								face		=> face);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_line (
								layer_cat	=> LAYER_CAT_ASSY,
								face		=> face);

						when SEC_STENCIL =>
							insert_line (
								layer_cat	=> LAYER_CAT_STENCIL,
								face		=> face);

						when SEC_STOPMASK =>
							insert_line (
								layer_cat	=> LAYER_CAT_STOPMASK,
								face		=> face);

						when SEC_KEEPOUT =>
							insert_line (
								layer_cat	=> LAYER_CAT_KEEPOUT,
								face		=> face);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_line;

				
				
				procedure build_non_conductor_arc (
					face : in et_pcb_sides.type_face)
				is begin
					board_check_arc (log_threshold + 1);
					
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_arc (
								layer_cat	=> LAYER_CAT_SILKSCREEN,
								face		=> face);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_arc (
								layer_cat	=> LAYER_CAT_ASSY,
								face		=> face);

						when SEC_STENCIL =>
							insert_arc (
								layer_cat	=> LAYER_CAT_STENCIL,
								face		=> face);

						when SEC_STOPMASK =>
							insert_arc (
								layer_cat	=> LAYER_CAT_STOPMASK,
								face		=> face);

						when SEC_KEEPOUT =>
							insert_arc (
								layer_cat	=> LAYER_CAT_KEEPOUT,
								face		=> face);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_arc;


				
				procedure build_non_conductor_circle (
					face : in et_pcb_sides.type_face)
				is
				begin
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_circle (
								layer_cat	=> LAYER_CAT_SILKSCREEN,
								face		=> face);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_circle (
								layer_cat	=> LAYER_CAT_ASSY,
								face		=> face);

						when SEC_STENCIL =>
							insert_circle (
								layer_cat	=> LAYER_CAT_STENCIL,
								face		=> face);

						when SEC_STOPMASK =>
							insert_circle (
								layer_cat	=> LAYER_CAT_STOPMASK,
								face		=> face);

						when SEC_KEEPOUT =>
							insert_circle (
								layer_cat	=> LAYER_CAT_KEEPOUT,
								face		=> face);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_circle;							


				
				procedure insert_netchanger (
					module_name	: in pac_module_name.bounded_string;
					module		: in out type_generic_module) 
				is
					inserted : boolean;
					use et_submodules;
					use pac_netchangers;
					cursor : pac_netchangers.cursor;
				begin
					log (text => "netchanger " & to_string (netchanger_id), level => log_threshold + 2);

					-- insert netchanger in container netchangers:
					insert (
						container	=> module.netchangers,
						key			=> netchanger_id,
						new_item	=> netchanger,
						inserted	=> inserted,
						position	=> cursor);

					-- A netchanger name must be unique:
					if not inserted then
						log (ERROR, "netchanger id" & to_string (netchanger_id) 
							& " already used !", console => true);
						raise constraint_error;
					end if;
					
					-- clean up for next netchanger
					netchanger_id := type_netchanger_id'first;
					netchanger := (others => <>);
				end insert_netchanger;

				
				

				
				
				procedure build_route_polygon is
					use et_board_geometry.pac_geometry_2;
					use et_board_geometry.pac_contours;
					use et_fill_zones;
					use et_fill_zones.boards;
					use et_thermal_relief;
					
					
					procedure solid_polygon is
						use pac_route_solid;

						procedure connection_thermal is
							p : type_route_solid (connection => THERMAL);
						begin
							load_segments (p, get_segments (contour));
							
							p.easing := board_easing;
							
							p.linewidth	:= polygon_width_min;
							p.isolation	:= polygon_isolation;
							
							p.properties.layer			:= signal_layer;
							p.properties.priority_level	:= contour_priority;
							p.relief_properties			:= relief_properties;

							pac_route_solid.append (
								container	=> route.zones.solid,
								new_item	=> p);
						end;

						
						procedure connection_solid is
							p : type_route_solid (connection => SOLID);
						begin
							load_segments (p, get_segments (contour));
							
							p.easing := board_easing;
							
							p.linewidth	:= polygon_width_min;
							p.isolation	:= polygon_isolation;
							
							p.properties.layer			:= signal_layer;
							p.properties.priority_level	:= contour_priority;
							p.technology				:= relief_properties.technology;

							pac_route_solid.append (
								container	=> route.zones.solid,
								new_item	=> p);
						end;

						
					begin -- solid_polygon
						case pad_connection is
							when THERMAL	=> connection_thermal;
							when SOLID		=> connection_solid;
						end case;
					end solid_polygon;


					
					procedure hatched_polygon is
						use pac_route_hatched;


						procedure connection_thermal is
							p : type_route_hatched (connection => THERMAL);
						begin
							load_segments (p, get_segments (contour));
							
							p.easing := board_easing;
							
							p.linewidth	:= polygon_width_min;
							p.isolation	:= polygon_isolation;
							
							p.properties.layer			:= signal_layer;
							p.properties.priority_level	:= contour_priority;
							p.relief_properties			:= relief_properties;
							
							pac_route_hatched.append (
								container	=> route.zones.hatched,
								new_item	=> p);
						end;

						
						procedure connection_solid is
							p : type_route_hatched (connection => SOLID);
						begin
							load_segments (p, get_segments (contour));
							
							p.easing := board_easing;
							
							p.linewidth	:= polygon_width_min;
							p.isolation	:= polygon_isolation;
							
							p.properties.layer			:= signal_layer;
							p.properties.priority_level	:= contour_priority;
							
							p.technology := relief_properties.technology;
							
							pac_route_hatched.append (
								container	=> route.zones.hatched,
								new_item	=> p);
						end;

						
					begin -- hatched_polygon
						case pad_connection is
							when THERMAL	=> connection_thermal;
							when SOLID		=> connection_solid;
						end case;
					end hatched_polygon;

					
				begin -- build_route_polygon
					case board_fill_style is
						when SOLID		=> solid_polygon;
						when HATCHED	=> hatched_polygon;
					end case;

					board_reset_contour; -- clean up for next polygon
				end build_route_polygon;


				-- This is now net specific restrict stuff !
				-- CS 			
				--procedure build_route_cutout is
					--use pac_contours;
					--use et_fill_zones.boards;
				--begin
					--pac_cutouts.append (
						--container	=> route.cutouts,
						--new_item	=> (contour with
										--layer	=> signal_layer));

					--board_reset_contour; -- clean up for next cutout zone
				--end build_route_cutout;

				
				procedure build_non_conductor_cutout (
					face	: in et_pcb_sides.type_face) 
				is begin
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_cutout (
								layer_cat	=> LAYER_CAT_SILKSCREEN,
								face		=> face);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_cutout (
								layer_cat	=> LAYER_CAT_ASSY,
								face		=> face);

						when SEC_STENCIL =>
							insert_cutout (
								layer_cat	=> LAYER_CAT_STENCIL,
								face		=> face);

						when SEC_STOPMASK =>
							insert_cutout (
								layer_cat	=> LAYER_CAT_STOPMASK,
								face		=> face);

						when SEC_KEEPOUT =>
							insert_cutout (
								layer_cat	=> LAYER_CAT_KEEPOUT,
								face		=> face);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_cutout;


				
				procedure build_non_conductor_fill_zone (
					face	: in et_pcb_sides.type_face)
				is begin
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_polygon (
								layer_cat	=> LAYER_CAT_SILKSCREEN,
								face		=> face);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_polygon (
								layer_cat	=> LAYER_CAT_ASSY,
								face		=> face);

						when SEC_STENCIL =>
							insert_polygon (
								layer_cat	=> LAYER_CAT_STENCIL,
								face		=> face);

						when SEC_STOPMASK =>
							insert_polygon (
								layer_cat	=> LAYER_CAT_STOPMASK,
								face		=> face);

						when SEC_KEEPOUT =>
							insert_polygon (
								layer_cat	=> LAYER_CAT_KEEPOUT,
								face		=> face);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_fill_zone;


				
				
				procedure build_non_conductor_text (
					face : in et_pcb_sides.type_face)  -- TOP, BOTTOM
				is
				-- The board_text has been a general thing until now. 
				-- Depending on the layer category and the side of the board (face) the board_text
				-- is now assigned to the board where it belongs to.
					
					procedure insert_text (
						layer_cat	: in type_layer_category)
					is					
						procedure do_it (
							module_name	: in pac_module_name.bounded_string;
							module		: in out type_generic_module) 
						is
							use et_pcb_sides;
							use et_board_geometry;
							use pac_geometry_2;
							use et_pcb;

							use et_silkscreen;
							use et_assy_doc;
							use et_stopmask;

						begin
							case face is
								when TOP =>
									case layer_cat is
										when LAYER_CAT_SILKSCREEN =>
											pac_silk_texts.append (
												container	=> module.board.silkscreen.top.texts,
												new_item	=> (board_text with null record));

										when LAYER_CAT_ASSY =>
											pac_doc_texts.append (
												container	=> module.board.assy_doc.top.texts,
												new_item	=> (board_text with null record));

										when LAYER_CAT_STOPMASK =>
											pac_stop_texts.append (
												container	=> module.board.stopmask.top.texts,
												new_item	=> (board_text with null record));

										when others => invalid_section;
									end case;
									
								when BOTTOM =>
									case layer_cat is
										when LAYER_CAT_SILKSCREEN =>
											pac_silk_texts.append (
												container	=> module.board.silkscreen.bottom.texts,
												new_item	=> (board_text with null record));

										when LAYER_CAT_ASSY =>
											pac_doc_texts.append (
												container	=> module.board.assy_doc.bottom.texts,
												new_item	=> (board_text with null record));

										when LAYER_CAT_STOPMASK =>
											pac_stop_texts.append (
												container	=> module.board.stopmask.bottom.texts,
												new_item	=> (board_text with null record));

										when others => invalid_section;
									end case;
									
							end case;
						end do_it;

						
					begin
						update_element (
							container	=> generic_modules,
							position	=> module_cursor,
							process		=> do_it'access);

						-- clean up for next board text
						board_text := (others => <>);
					end insert_text;

					
				begin -- build_non_conductor_text
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_text (LAYER_CAT_SILKSCREEN);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_text (LAYER_CAT_ASSY);

						when SEC_STENCIL =>
							insert_text (LAYER_CAT_STENCIL);
							
						when SEC_STOPMASK =>
							insert_text (LAYER_CAT_STOPMASK);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_text;


				
				procedure build_net_label is
					use et_schematic_text;
					use pac_text_schematic;
					use et_net_labels;
				begin
					case stack.parent is
						when SEC_LABELS =>

							-- insert label in label collection

							-- insert a simple label
							pac_net_labels.append (
								container	=> net_labels,
								new_item	=> net_label);

							-- clean up for next label
							net_label := (others => <>);

						when others => invalid_section;
					end case;
				end build_net_label;
					
				
				
				
			begin -- execute_section
				case stack.current is

					when SEC_CONNECTORS =>
						case stack.parent is
							when SEC_SEGMENT =>
								assign_net_connectors;

							when others => invalid_section;
						end case;

					
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_ZONE => check_outline (contour, log_threshold + 1);
							when SEC_CUTOUT_ZONE => check_outline (contour, log_threshold + 1);
							
							when others => invalid_section;
						end case;


					when SEC_JUNCTIONS =>
						case stack.parent is
							when SEC_SEGMENT =>
								assign_net_junctions;

							when others => invalid_section;
						end case;
						
						
					when SEC_NET_CLASS =>
						case stack.parent is
							when SEC_NET_CLASSES =>
								assign_net_class (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_NET_CLASSES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_BOARD_LAYER_STACK =>
						case stack.parent is
							when SEC_INIT =>
								add_board_layer;

							when others => invalid_section;
						end case;

						
					when SEC_DRAWING_GRID =>
						case stack.parent is
							when SEC_INIT => 
								set_drawing_grid (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_NET =>
						case stack.parent is
							when SEC_NETS =>
								assign_net;
						
							when others => invalid_section;
						end case;

						
					when SEC_NETS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_STRANDS =>
						case stack.parent is
							when SEC_NET =>

								-- insert strand collection in net
								net.strands := strands;
								pac_strands.clear (strands); -- clean up for next strand collection

							when others => invalid_section;
						end case;

						
					when SEC_ROUTE =>
						case stack.parent is
							when SEC_NET =>

								-- insert route in net
								net.route := route;
								route := (others => <>); -- clean up route for next net
								
							when others => invalid_section;
						end case;

						
					when SEC_STRAND =>
						case stack.parent is
							when SEC_STRANDS =>
								assign_net_strand;
								
							when others => invalid_section;
						end case;

						
					when SEC_SEGMENTS =>
						case stack.parent is
							when SEC_STRAND =>

								-- insert segments in strand
								strand.segments := net_segments;

								-- clean up for next segment collection
								et_net_segment.pac_net_segments.clear (net_segments);
								
							when others => invalid_section;
						end case;

						
					when SEC_SEGMENT =>
						case stack.parent is
							when SEC_SEGMENTS =>
								assign_net_segment;
								
							when others => invalid_section;
						end case;

						
					when SEC_LABELS =>
						case stack.parent is
							when SEC_SEGMENT =>
								assign_net_labels;

							when others => invalid_section;
						end case;

						
					when SEC_PORTS =>
						case stack.parent is
							when SEC_SEGMENT =>
								assign_net_ports;


							when SEC_SUBMODULE =>
								-- copy collection of ports to submodule
								submodule.ports := submodule_ports;

								-- clean up for next collection of ports
								et_submodules.pac_submodule_ports.clear (submodule_ports);
								
							when others => invalid_section;
						end case;

						
					when SEC_LABEL =>
						build_net_label;
						
					when SEC_LINE => -- CS clean up. separate procedures required
						case stack.parent is
							when SEC_CONTOURS => add_polygon_line (board_line);
								
							when SEC_ROUTE =>

								-- insert line in route.lines
								et_conductor_segment.boards.pac_conductor_lines.append (
									container	=> route.lines,
									new_item	=> (et_board_geometry.pac_geometry_2.type_line (board_line) with
											width	=> board_line_width,
											layer	=> signal_layer));
									
								board_reset_line;
								board_reset_line_width;
								board_reset_signal_layer;

							when SEC_TOP =>
								build_non_conductor_line (et_pcb_sides.TOP);

							when SEC_BOTTOM =>
								build_non_conductor_line (et_pcb_sides.BOTTOM);

							when SEC_ROUTE_RESTRICT =>
								insert_line_route_restrict;

							when SEC_CONDUCTOR =>
								insert_line_track;

							when SEC_OUTLINE =>
								insert_line_outline;

							when SEC_HOLE =>
								add_polygon_line (board_line);
								
							when others => invalid_section;
						end case;
						
						
					when SEC_ARC => -- CS clean up. separate procedures required
						case stack.parent is
							when SEC_CONTOURS => 
								board_check_arc (log_threshold + 1);
								add_polygon_arc (board_arc);

							when SEC_ROUTE =>
								board_check_arc (log_threshold + 1);
								
								-- insert arc in route.arcs
								et_conductor_segment.boards.pac_conductor_arcs.append (
									container	=> route.arcs,
									new_item	=> (et_board_geometry.pac_geometry_2.type_arc (board_arc) with
											width	=> board_line_width,
											layer	=> signal_layer));
									
								board_reset_arc;
								board_reset_line_width;
								board_reset_signal_layer;
								
							when SEC_TOP =>
								build_non_conductor_arc (et_pcb_sides.TOP);

							when SEC_BOTTOM =>
								build_non_conductor_arc (et_pcb_sides.BOTTOM);

							when SEC_ROUTE_RESTRICT =>
								board_check_arc (log_threshold + 1);
								insert_arc_route_restrict;

							when SEC_CONDUCTOR =>
								board_check_arc (log_threshold + 1);
								insert_arc_track;

							when SEC_OUTLINE =>
								board_check_arc (log_threshold + 1);
								insert_arc_outline;

							when SEC_HOLE =>
								add_polygon_arc (board_arc);
								
							when others => invalid_section;
						end case;

						
					when SEC_CIRCLE => -- CS clean up. separate procedures required
						case stack.parent is
							when SEC_CONTOURS => add_polygon_circle (board_circle);
							
							when SEC_TOP =>
								build_non_conductor_circle (et_pcb_sides.TOP);

							when SEC_BOTTOM =>
								build_non_conductor_circle (et_pcb_sides.BOTTOM);

							when SEC_ROUTE_RESTRICT =>
								insert_circle_route_restrict;

							when SEC_CONDUCTOR =>
								insert_circle_track;

							when SEC_OUTLINE =>
								insert_circle_outline;

							when SEC_HOLE =>
								add_polygon_circle (board_circle);
								
							when others => invalid_section;
						end case;

						
					when SEC_VIA =>
						case stack.parent is
							when SEC_ROUTE =>
								build_via;
						
							when others => invalid_section;
						end case;

						
					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							--when SEC_ROUTE =>
								--build_route_cutout;

							when SEC_TOP =>
								build_non_conductor_cutout (et_pcb_sides.TOP);

							when SEC_BOTTOM =>
								build_non_conductor_cutout (et_pcb_sides.BOTTOM);

							when SEC_ROUTE_RESTRICT =>
								insert_cutout_route_restrict;

							when SEC_VIA_RESTRICT =>
								insert_cutout_via_restrict;

							when SEC_CONDUCTOR =>
								insert_cutout_conductor;
								
							when others => invalid_section;
						end case;

						
					when SEC_ZONE =>
						case stack.parent is
							when SEC_ROUTE =>
								build_route_polygon;

							when SEC_TOP =>
								build_non_conductor_fill_zone (et_pcb_sides.TOP);
						
							when SEC_BOTTOM =>
								build_non_conductor_fill_zone (et_pcb_sides.BOTTOM);

							when SEC_ROUTE_RESTRICT =>
								insert_polygon_route_restrict;

							when SEC_VIA_RESTRICT =>
								insert_zone_via_restrict;

							when SEC_CONDUCTOR =>
								insert_polygon_conductor;
								
							when others => invalid_section;
						end case;

						
					when SEC_SUBMODULE =>
						case stack.parent is
							when SEC_SUBMODULES =>

								-- insert submodule
								update_element (
									container	=> generic_modules,
									position	=> module_cursor,
									process		=> insert_submodule'access);

							when others => invalid_section;
						end case;

						
					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS =>
								case stack.parent (degree => 2) is
									when SEC_SUBMODULE => insert_submodule_port;
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
					when SEC_SUBMODULES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_SCHEMATIC =>
						case stack.parent is
							when SEC_DRAWING_FRAMES =>
								set_frame_schematic;


							when SEC_DRAWING_GRID => null; -- nothing to do

							when SEC_META =>
								add_meta_schematic;


							when others => invalid_section;
						end case;

						
					when SEC_BOARD =>
						case stack.parent is
							when SEC_INIT => null;

							when SEC_DRAWING_FRAMES =>
								set_frame_board;
								
							when SEC_DRAWING_GRID => null; -- nothing to do

							when SEC_META =>
								add_meta_board;
							
							when others => invalid_section;
						end case;

						
					when SEC_SHEET_DESCRIPTIONS =>
						case stack.parent is
							when SEC_SCHEMATIC => null; 
								-- We assign the sheet_descriptions once parent 
								-- section SCHEMATIC closes.
								-- See procdure set_frame_schematic.

							when others => invalid_section;
						end case;

						
					when SEC_SHEET =>
						case stack.parent is
							when SEC_SHEET_DESCRIPTIONS => add_sheet_description;
							when others => invalid_section;
						end case;

						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>

								-- insert note
								update_element (
									container	=> generic_modules,
									position	=> module_cursor,
									process		=> insert_schematic_text'access);

							when SEC_TOP =>
								build_non_conductor_text (et_pcb_sides.TOP);
						
							when SEC_BOTTOM =>
								build_non_conductor_text (et_pcb_sides.BOTTOM);

							when SEC_CONDUCTOR =>
								build_conductor_text;

							when others => invalid_section;
						end case;

						
					when SEC_TEXTS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_DRAWING_FRAMES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								case stack.parent (degree => 2) is
									when SEC_DEVICE =>
										et_module_read_device_non_electrical.insert_package_placeholder;
										
									when SEC_PACKAGE =>
										et_module_read_device_electrical.insert_package_placeholder;

									when SEC_UNIT =>
										build_unit_placeholder;

									when others => invalid_section;
								end case;

								
							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>
										insert_placeholder (
											layer_cat	=> LAYER_CAT_SILKSCREEN,
											face		=> et_pcb_sides.TOP);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_placeholder (
											layer_cat	=> LAYER_CAT_ASSY,
											face		=> et_pcb_sides.TOP);

									when SEC_STOPMASK =>
										insert_placeholder (
											layer_cat	=> LAYER_CAT_STOPMASK,
											face		=> et_pcb_sides.TOP);

									when others => invalid_section;
								end case;

								
							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>
										insert_placeholder (
											layer_cat	=> LAYER_CAT_SILKSCREEN,
											face		=> et_pcb_sides.BOTTOM);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_placeholder (
											layer_cat	=> LAYER_CAT_ASSY,
											face		=> et_pcb_sides.BOTTOM);

									when SEC_STOPMASK =>
										insert_placeholder (
											layer_cat	=> LAYER_CAT_STOPMASK,
											face		=> et_pcb_sides.BOTTOM);

									when others => invalid_section;
								end case;

								
							when SEC_CONDUCTOR =>
								insert_board_text_placeholder;
								
							when others => invalid_section;
						end case;

						
					when SEC_PLACEHOLDERS =>
						case stack.parent is
							when SEC_PACKAGE =>
								et_module_read_device_electrical.insert_placeholders;
								

							when SEC_DEVICE =>
								case stack.parent (degree => 2) is
									when SEC_DEVICES_NON_ELECTRIC =>
										et_module_read_device_non_electrical.insert_placeholders;

									when others => invalid_section;
								end case;
								
							when SEC_UNIT => null;
								
							when others => invalid_section;
						end case;

						
					when SEC_PACKAGE =>
						case stack.parent is
							when SEC_DEVICE =>
								set_package_position;

							when others => invalid_section;
						end case;

						
					when SEC_UNIT =>
						case stack.parent is
							when SEC_UNITS =>
								insert_unit (module_cursor, log_threshold);
													
							when others => invalid_section;
						end case;

						
					when SEC_UNITS =>
						case stack.parent is
							when SEC_DEVICE =>
								insert_units;
								
							when others => invalid_section;
						end case;

						
					when SEC_DEVICE =>
						case stack.parent is
							when SEC_DEVICES =>
								insert_device (module_cursor, log_threshold);

							when SEC_DEVICES_NON_ELECTRIC => 
								insert_device_non_electrical (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_DEVICES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_ASSEMBLY_VARIANT =>
						case stack.parent is
							when SEC_ASSEMBLY_VARIANTS => 
								insert_assembly_variant (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_ASSEMBLY_VARIANTS =>
						case stack.parent is
							when SEC_INIT => null; -- CS test if active variant exists
							when others => invalid_section;
						end case;

						
					when SEC_META =>
						case stack.parent is
							when SEC_INIT => 
								set_meta (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_PREFERRED_LIBRARIES =>
						case stack.parent is
							when SEC_SCHEMATIC =>
								case stack.parent (degree => 2) is
									when SEC_META	=> null; -- nothing to do
									when others		=> invalid_section;
								end case;

							when SEC_BOARD =>
								case stack.parent (degree => 2) is
									when SEC_META	=> null; -- nothing to do
									when others		=> invalid_section;
								end case;
								
							when others => invalid_section;
						end case;

						
					when SEC_RULES =>
						case stack.parent is
							when SEC_INIT => 
								set_rules (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_NETCHANGERS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_NETCHANGER =>
						case stack.parent is
							when SEC_NETCHANGERS =>

								-- insert netchanger in module
								update_element (
									container	=> generic_modules,
									position	=> module_cursor,
									process		=> insert_netchanger'access);
								
							when others => invalid_section;
						end case;
						
					when SEC_DEVICES_NON_ELECTRIC | SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
						SEC_STOPMASK | SEC_KEEPOUT | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT |
						SEC_CONDUCTOR | SEC_PCB_CONTOURS_NON_PLATED =>
						case stack.parent is
							when SEC_BOARD => null;
							when others => invalid_section;
						end case;

					when SEC_TOP | SEC_BOTTOM =>
						case stack.parent is
							when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
								SEC_STOPMASK | SEC_KEEPOUT => null;

							when others => invalid_section;
						end case;

					when SEC_USER_SETTINGS =>
						case stack.parent is
							when SEC_BOARD		=> assign_user_settings_board;
							-- CS when SEC_SCHEMATIC	=> null;
							
							when others => invalid_section;
						end case;

					when SEC_VIAS | SEC_FILL_ZONES_CONDUCTOR =>
						case stack.parent is
							when SEC_USER_SETTINGS =>
								case stack.parent (degree => 2) is
									when SEC_BOARD	=> null;
									when others		=> invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_OUTLINE =>
						case stack.parent is
							when SEC_PCB_CONTOURS_NON_PLATED => null;
							when others => invalid_section;
						end case;

					when SEC_HOLE =>
						case stack.parent is
							when SEC_PCB_CONTOURS_NON_PLATED =>
								append_hole;
								
							when others => invalid_section;
						end case;

						
					when SEC_INIT => null; -- CS: should never happen
				end case;

	-- CS:
	-- 				exception when event:
	-- 					others => 
	-- 						log (text => ada.exceptions.exception_message (event), console => true);
	-- 						raise;
				
			end execute_section;


			
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
			function set (
				section_keyword	: in string; -- [NETS
				section			: in type_module_section) -- SEC_NETS
				return boolean is 
			begin -- set
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 5);
						return true;

					elsif f (line, 2) = section_end then -- section footer detected in field 2

						-- The section name in the footer must match the name
						-- of the current section. Otherwise abort.
						if section /= stack.current then
							log_indentation_reset;
							invalid_section;
						end if;
						
						-- Now that the section ends, the data collected in temporarily
						-- variables is processed.
						log_indentation_up;
						execute_section;
						log_indentation_down;
						
						stack.pop;
						if stack.empty then
							log (text => write_top_level_reached, level => log_threshold + 5);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 5);
						end if;
						return true;

					else
						log (ERROR, write_missing_begin_end, console => true);
						raise constraint_error;
					end if;

				else -- neither a section header nor footer
					return false;
				end if;
			end set;


			use et_device_read;
			use et_package_sections;
			use et_device_sections;
		
			
		begin -- process_line
			if set (section_net_classes, SEC_NET_CLASSES) then null;
			elsif set (section_connectors, SEC_CONNECTORS) then null;
			elsif set (section_net_class, SEC_NET_CLASS) then null;
			elsif set (section_junctions, SEC_JUNCTIONS) then null;
			elsif set (section_board_layer_stack, SEC_BOARD_LAYER_STACK) then null;			
			elsif set (section_drawing_grid, SEC_DRAWING_GRID) then null;
			elsif set (section_nets, SEC_NETS) then null;
			elsif set (section_net, SEC_NET) then null;
			elsif set (section_strands, SEC_STRANDS) then null;
			elsif set (section_strand, SEC_STRAND) then null;
			elsif set (section_segments, SEC_SEGMENTS) then null;
			elsif set (section_segment, SEC_SEGMENT) then null;
			elsif set (section_labels, SEC_LABELS) then null;
			elsif set (section_label, SEC_LABEL) then null;
			elsif set (section_fill_zones_conductor, SEC_FILL_ZONES_CONDUCTOR) then null;
			elsif set (section_ports, SEC_PORTS) then null;
			elsif set (section_port, SEC_PORT) then null;				
			elsif set (section_route, SEC_ROUTE) then null;								
			elsif set (section_line, SEC_LINE) then null;								
			elsif set (section_arc, SEC_ARC) then null;
			elsif set (section_cutout_zone, SEC_CUTOUT_ZONE) then null;
			elsif set (section_zone, SEC_ZONE) then null;								
			elsif set (section_contours, SEC_CONTOURS) then null;								
			elsif set (section_via, SEC_VIA) then null;								
			elsif set (section_submodules, SEC_SUBMODULES) then null;
			elsif set (section_submodule, SEC_SUBMODULE) then null;
			elsif set (section_drawing_frames, SEC_DRAWING_FRAMES) then null;
			elsif set (section_schematic, SEC_SCHEMATIC) then null;
			elsif set (section_sheet_descriptions, SEC_SHEET_DESCRIPTIONS) then null;
			elsif set (section_sheet, SEC_SHEET) then null;
			elsif set (section_board, SEC_BOARD) then null;
			elsif set (section_devices, SEC_DEVICES) then null;
			elsif set (section_device, SEC_DEVICE) then null;
			elsif set (section_devices_non_electric, SEC_DEVICES_NON_ELECTRIC) then null;
			elsif set (section_units, SEC_UNITS) then null;
			elsif set (section_unit, SEC_UNIT) then null;
			elsif set (section_assembly_variants, SEC_ASSEMBLY_VARIANTS) then null;
			elsif set (section_assembly_variant, SEC_ASSEMBLY_VARIANT) then null;
			elsif set (section_netchangers, SEC_NETCHANGERS) then null;
			elsif set (section_netchanger, SEC_NETCHANGER) then null;
			elsif set (section_meta, SEC_META) then null;
			elsif set (section_preferred_libraries, SEC_PREFERRED_LIBRARIES) then null;
			elsif set (section_rules, SEC_RULES) then null;			
			elsif set (section_placeholders, SEC_PLACEHOLDERS) then null;				
			elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
			elsif set (section_package, SEC_PACKAGE) then null;
			elsif set (section_texts, SEC_TEXTS) then null;
			elsif set (section_text, SEC_TEXT) then null;
			elsif set (section_silkscreen, SEC_SILKSCREEN) then null;
			elsif set (section_top, SEC_TOP) then null;
			elsif set (section_bottom, SEC_BOTTOM) then null;
			elsif set (section_circle, SEC_CIRCLE) then null;
			elsif set (section_assembly_doc, SEC_ASSEMBLY_DOCUMENTATION) then null;
			elsif set (section_stencil, SEC_STENCIL) then null;
			elsif set (section_stopmask, SEC_STOPMASK) then null;
			elsif set (section_keepout, SEC_KEEPOUT) then null;
			elsif set (section_route_restrict, SEC_ROUTE_RESTRICT) then null;
			elsif set (section_via_restrict, SEC_VIA_RESTRICT) then null;
			elsif set (section_conductor, SEC_CONDUCTOR) then null;				
			elsif set (section_pcb_contours, SEC_PCB_CONTOURS_NON_PLATED) then null;
			elsif set (section_hole, SEC_HOLE) then null;
			elsif set (section_outline, SEC_OUTLINE) then null;
			elsif set (section_user_settings, SEC_USER_SETTINGS) then null;
			elsif set (section_vias, SEC_VIAS) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "module line --> " & to_string (line), level => log_threshold + 4);
				log_indentation_up;

				
				case stack.current is

					when SEC_CONNECTORS =>
						case stack.parent is
							when SEC_SEGMENT => read_net_connector;
							when others => invalid_section;
						end case;
					
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_ZONE => null;
							when SEC_CUTOUT_ZONE => null;
							when others => invalid_section;
						end case;

					when SEC_JUNCTIONS =>
						case stack.parent is
							when SEC_SEGMENT => read_net_junction;
							when others => invalid_section;
						end case;
						
					when SEC_NET_CLASSES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_DRAWING_GRID =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_BOARD_LAYER_STACK =>
						case stack.parent is
							when SEC_INIT => read_layer;
							when others => invalid_section;
						end case;
						
					when SEC_DEVICES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_ASSEMBLY_VARIANTS =>
						case stack.parent is
							when SEC_INIT => 
								set_active_assembly_variant (module_cursor, line, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_ASSEMBLY_VARIANT =>
						case stack.parent is
							when SEC_ASSEMBLY_VARIANTS => 
								read_assembly_variant (module_cursor, line);
								
							when others => invalid_section;
						end case;

						
					when SEC_TEXTS =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do								
							when others => invalid_section;
						end case;

					when SEC_SUBMODULES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do								
							when others => invalid_section;
						end case;

					when SEC_DRAWING_FRAMES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do								
							when others => invalid_section;
						end case;
						
					when SEC_META =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_PREFERRED_LIBRARIES =>
						case stack.parent is
							when SEC_SCHEMATIC =>
								case stack.parent (degree => 2) is
									when SEC_META => 
										read_preferred_lib_schematic (line);
										
									when others => invalid_section;
								end case;

							when SEC_BOARD =>
								case stack.parent (degree => 2) is
									when SEC_META =>
										read_preferred_lib_board (line);
										
									when others	=> invalid_section;
								end case;
								
							when others => invalid_section;
						end case;

						
					when SEC_RULES =>
						case stack.parent is
							when SEC_INIT => read_rules (line);
							when others => invalid_section;
						end case;
						
					when SEC_NET_CLASS =>
						case stack.parent is
							when SEC_NET_CLASSES => read_net_class (line, log_threshold);
							when others => invalid_section;
						end case;

					when SEC_STRAND =>
						case stack.parent is
							when SEC_STRANDS => read_strand;
							when others => invalid_section;
						end case;
						
					when SEC_STRANDS =>
						case stack.parent is
							when SEC_NET => null; -- nothing to do
							when others => invalid_section;
						end case;
					
					when SEC_ROUTE =>
						case stack.parent is
							when SEC_NET => null; -- nothing to do
							when others => invalid_section;
						end case;
					
					when SEC_NET =>
						case stack.parent is
							when SEC_NETS => read_net;
							when others => invalid_section;
						end case;

					when SEC_NETS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;
						
					when SEC_SEGMENT =>
						case stack.parent is
							when SEC_SEGMENTS => read_net_segment;
							when others => invalid_section;
						end case;

					when SEC_SEGMENTS =>
						case stack.parent is
							when SEC_STRAND => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_LABELS =>
						case stack.parent is
							when SEC_SEGMENT => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PORTS =>
						case stack.parent is 
							when SEC_SEGMENT => read_net_port;
							when SEC_SUBMODULE => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_LABEL =>
						case stack.parent is
							when SEC_LABELS => read_label;
							when others => invalid_section;
						end case;
					
					when SEC_LINE => -- CS clean up: separate procdures required
						case stack.parent is
							when SEC_CONTOURS => read_board_line (line); -- of a cutout or fill zone
								
							when SEC_ROUTE =>
								if not read_board_line (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layer then -- layer 2
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
											validate_signal_layer;

										elsif kw = keyword_width then -- width 0.5
											expect_field_count (line, 2);
											board_line_width := et_board_geometry.pac_geometry_2.to_distance (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK =>

										if not read_board_line (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := et_board_geometry.pac_geometry_2.to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_KEEPOUT => read_board_line (line);
										
									when others => invalid_section;
								end case;
								
							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>							
								if not read_board_line (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layers then -- layers 1 14 3

											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);
											signal_layers := to_layers (line, check_layers);
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
								
							when SEC_CONDUCTOR =>
								if not read_board_line (line) then
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_width then -- width 0.5
											expect_field_count (line, 2);
											board_line_width := et_board_geometry.pac_geometry_2.to_distance (f (line, 2));

										elsif kw = keyword_layer then -- layer 1
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
											validate_signal_layer;

										else
											invalid_keyword (kw);
										end if;
									end;
								end if;

							when SEC_OUTLINE | SEC_HOLE =>
								read_board_line (line);
								
							when others => invalid_section;
						end case;

					when SEC_ARC =>  -- CS clean up: separate procdures required
						case stack.parent is
							when SEC_CONTOURS => read_board_arc (line);
							
							when SEC_ROUTE =>
								if not read_board_arc (line) then
									declare
										kw : string := f (line, 1);
										use et_board_geometry.pac_geometry_2;
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layer then -- layer 2
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
											validate_signal_layer;
											
										elsif kw = keyword_width then -- width 0.5
											expect_field_count (line, 2);
											board_line_width := et_board_geometry.pac_geometry_2.to_distance (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;
									
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK =>

										if not read_board_arc (line) then
										
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := et_board_geometry.pac_geometry_2.to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;

										end if;
										
									when SEC_KEEPOUT => read_board_arc (line);
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								if not read_board_arc (line) then
								
									declare
										kw : string := f (line, 1);
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_layers then -- layers 1 14 3

											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);

											signal_layers := to_layers (line, check_layers);

										else
											invalid_keyword (kw);
										end if;
									end;

								end if;
								
							when SEC_CONDUCTOR =>
								if not read_board_arc (line) then
									declare
										kw : string := f (line, 1);
										use et_board_geometry.pac_geometry_2;
										use et_pcb_stack;
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_width then -- width 0.5
											expect_field_count (line, 2);
											board_line_width := et_board_geometry.pac_geometry_2.to_distance (f (line, 2));

										elsif kw = keyword_layer then -- layer 1
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
											validate_signal_layer;
											
										else
											invalid_keyword (kw);
										end if;
									end;
								end if;

							when SEC_OUTLINE | SEC_HOLE =>
								read_board_arc (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_CIRCLE => -- CS clean up: separate procdures required
						case stack.parent is
							when SEC_CONTOURS => read_board_circle (line);
							
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK =>

										if not read_board_circle (line) then
										
											declare -- CS separate procedure
												use et_board_geometry;
												use pac_geometry_2;
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- circumfence line width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));
												else
													invalid_keyword (kw);
												end if;
											end;

										end if;
										
									when SEC_KEEPOUT =>
										if not read_board_circle (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_filled then -- filled yes/no
													expect_field_count (line, 2);													
													board_filled := to_filled (f (line, 2));
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when others => invalid_section;

								end case;

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								if not read_board_circle (line) then

									declare
										use et_pcb_stack;
										use et_board_geometry.pac_geometry_2;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_filled then -- filled yes/no
											expect_field_count (line, 2);													
											board_filled := to_filled (f (line, 2));

										elsif kw = keyword_layers then -- layers 1 14 3

											-- there must be at least two fields:
											expect_field_count (line => line, count_expected => 2, warn => false);

											signal_layers := to_layers (line, check_layers);
											
										else
											invalid_keyword (kw);
										end if;
									end;

								end if;
								
							when SEC_CONDUCTOR =>
								if not read_board_circle (line) then
									declare -- CS separate procdure
										use et_pcb_stack;
										use et_board_geometry.pac_geometry_2;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_width then -- width 0.5
											expect_field_count (line, 2);
											board_line_width := to_distance (f (line, 2));
											
										elsif kw = keyword_layer then -- layer 1
											expect_field_count (line, 2);
											signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
											validate_signal_layer;

										else
											invalid_keyword (kw);
										end if;
									end;
								end if;

							when SEC_OUTLINE | SEC_HOLE =>
								read_board_circle (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_ROUTE => read_cutout_route;
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK => read_cutout_non_conductor;

									when SEC_KEEPOUT =>
										-- no parameters allowed here
										declare
											kw : string := f (line, 1);
										begin
											invalid_keyword (kw);
										end;
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT => read_cutout_restrict;
							when SEC_CONDUCTOR => read_cutout_conductor_non_electric;
							when others => invalid_section;
						end case;

						
					when SEC_ZONE =>
						case stack.parent is
							when SEC_ROUTE => read_fill_zone_route;

							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK => read_fill_zone_non_conductor;

									when SEC_KEEPOUT => read_fill_zone_keepout;
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT => read_fill_zone_restrict;
							when SEC_CONDUCTOR => read_fill_zone_conductor_non_electric;
							when others => invalid_section;
						end case;

						
					when SEC_VIA =>
						case stack.parent is
							when SEC_ROUTE	=> read_via;
							when others		=> invalid_section;
						end case;

						
					when SEC_SUBMODULE =>
						case stack.parent is
							when SEC_SUBMODULES => read_submodule;
							when others => invalid_section;
						end case;

						
					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS =>
								case stack.parent (degree => 2) is
									when SEC_SUBMODULE => read_submodule_port;
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
					when SEC_SCHEMATIC =>
						case stack.parent is
							when SEC_DRAWING_FRAMES => read_frame_template_schematic;
							
							when SEC_DRAWING_GRID => 
								read_drawing_grid_schematic (line);
								
							when SEC_META => read_meta_schematic (line);
							when others => invalid_section;
						end case;

						
					when SEC_BOARD =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when SEC_DRAWING_FRAMES => read_frame_template_board;
							
							when SEC_DRAWING_GRID => 
								read_drawing_grid_board (line);
								
							when SEC_META => read_meta_board (line);
							when others => invalid_section;
						end case;

						
					when SEC_SHEET_DESCRIPTIONS =>
						case stack.parent is
							when SEC_SCHEMATIC => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_SHEET =>
						case stack.parent is
							when SEC_SHEET_DESCRIPTIONS => read_sheet_description;
							when others => invalid_section;
						end case;

						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS => -- in schematic
								read_schematic_text;

							when SEC_PCB_CONTOURS_NON_PLATED => -- in board
								read_board_text_contours;
								
							when SEC_TOP | SEC_BOTTOM => -- in board
								read_board_text_non_conductor;

							when SEC_CONDUCTOR | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								read_board_text_conductor;
								
							when others => invalid_section;
						end case;

						
					when SEC_DEVICE =>
						case stack.parent is
							when SEC_DEVICES => read_device (line);
							when SEC_DEVICES_NON_ELECTRIC => read_device_non_electric (line);
							when others => invalid_section;
						end case;

						
					when SEC_PACKAGE =>
						case stack.parent is
							when SEC_DEVICE =>
								et_module_read_device_electrical.read_package_position (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								case stack.parent (degree => 2) is
									when SEC_DEVICE => 
										et_module_read_device_non_electrical.read_device_text_placeholder (line);
									
									when SEC_PACKAGE => 
										et_module_read_device_electrical.read_device_text_placeholder (line);
										
									when SEC_UNIT => read_unit_placeholder (line);
									when others => invalid_section;
								end case;

								
							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION 
										| SEC_STOPMASK => -- CS SEC_KEEPOUT
										read_board_text_placeholder;
							
									when others => invalid_section;
								end case;

							when SEC_CONDUCTOR => read_board_text_conductor_placeholder;
								
							when others => invalid_section;
						end case;

						
					when SEC_PLACEHOLDERS =>
						case stack.parent is
							when SEC_PACKAGE => null;

							when SEC_DEVICE =>
								case stack.parent (degree => 2) is
									when SEC_DEVICES_NON_ELECTRIC => null;

									when others => invalid_section;
								end case;
							
							when SEC_UNIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_UNIT =>
						case stack.parent is
							when SEC_UNITS => read_unit (line);
							when others => invalid_section;
						end case;

						
					when SEC_UNITS =>
						case stack.parent is
							when SEC_DEVICE => null;
							when others => invalid_section;
						end case;

						
					when SEC_NETCHANGERS =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_NETCHANGER =>
						case stack.parent is
							when SEC_NETCHANGERS => read_netchanger;
							when others => invalid_section;
						end case;

						
					when SEC_DEVICES_NON_ELECTRIC | SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
						SEC_STOPMASK | SEC_KEEPOUT | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT |
						SEC_CONDUCTOR | SEC_PCB_CONTOURS_NON_PLATED =>
						case stack.parent is
							when SEC_BOARD => null;
							when others => invalid_section;
						end case;

						
					when SEC_TOP | SEC_BOTTOM =>
						case stack.parent is
							when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
								SEC_STOPMASK | SEC_KEEPOUT => null;

							when others => invalid_section;
						end case;

						
					when SEC_USER_SETTINGS =>
						case stack.parent is
							when SEC_BOARD		=> null;
							-- CS when SEC_SCHEMATIC	=> null;
							
							when others => invalid_section;
						end case;

						
					when SEC_VIAS =>
						case stack.parent is
							when SEC_USER_SETTINGS =>
								case stack.parent (degree => 2) is
									when SEC_BOARD	=> read_user_settings_vias;								
									when others		=> invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
					when SEC_FILL_ZONES_CONDUCTOR =>
						case stack.parent is
							when SEC_USER_SETTINGS =>
								case stack.parent (degree => 2) is
									when SEC_BOARD	=> read_user_settings_fill_zones_conductor;
									when others		=> invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
					when SEC_OUTLINE | SEC_HOLE =>
						case stack.parent is
							when SEC_PCB_CONTOURS_NON_PLATED => null;
							when others => invalid_section;
						end case;

						
					when SEC_INIT => null; -- CS: should never happen
				end case;


				log_indentation_down;
			end if;

			exception when event: others =>
				log (text => "file " & file_name & space 
					& get_affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;




		
		-- This procdure reads the module file line per line
		-- and adds the content to the module. Cursor module_cursor
		-- points to the module:
		procedure read_module_file is begin

			-- Open the module file and direct the input accordingly:
			open (
				file => file_handle,
				mode => in_file, 
				name => file_name_expanded);
			
			set_input (file_handle);
			
			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);

			
			-- Read the file line by line:
			while not end_of_file loop
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> positive (ada.text_io.line (current_input)),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- We are interested only in lines that 
				-- contain something. emtpy lines are skipped:
				if get_field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			
			-- As a safety measure the top section must be reached finally:
			if stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			-- Close the module file and set the input back to standard input:
			set_input (previous_input);
			close (file_handle);

			-- Compute the ratsnest (airwires):
			et_board_ops.ratsnest.update_ratsnest (module_cursor, log_threshold + 2);
		end read_module_file;



		
		
		procedure read_submodule_files is
		-- Pointer module_cursor points to the last module that has been read.
		-- Take a copy of the submodules stored in module.submods. 
		-- Then iterate in that copy (submods) to read the actual 
		-- module files (like templates/clock_generator.mod).
		-- NOTE: The parent procedure "read_module" calls itself here !

			use et_submodules;
			use pac_submodules;
			use ada.containers;

			-- Here the copy of submodules lives:
			submods : et_submodules.pac_submodules.map;

			
			procedure get_submodules (
			-- Copies the submodules in submods.
				module_name	: pac_module_name.bounded_string;
				module		: type_generic_module) is
			begin
				submods := module.submods;
			end;

			
			procedure query_module (cursor : in pac_submodules.cursor) is begin
				-- Read the template file:
				read_module (to_string (element (cursor).file), log_threshold + 1);
			end;

			
		begin -- read_submodule_files
			-- take a copy of submodules
			query_element (
				position	=> module_cursor,
				process		=> get_submodules'access);

			if length (submods) > 0 then
				log (text => "submodules/templates ...", level => log_threshold);
				log_indentation_up;
			
				-- Query submodules of the parent module (accessed by module_cursor):
				iterate (submods, query_module'access);

				log_indentation_down;
			end if;

		end read_submodule_files;



		
		use ada.directories;
		
		
	begin
		log (text => "opening module file " & enclose_in_quotes (file_name) & " ...", level => log_threshold);
		--log (text => "full name " & enclose_in_quotes (file_name_expanded), level => log_threshold + 1);
		log_indentation_up;
		
		-- Make sure the module file exists.
		-- The file_name may contain environment variables (like $templates). 
		-- In order to test whether the given module file exists, file name_name must be expanded
		-- so that the environment variables are replaced by the real paths like:
		-- templates/clock_generator.mod or
		-- /home/user/et_templates/pwr_supply.mod.
		if exists (file_name_expanded) then

			log (text => "expanded name: " & enclose_in_quotes (full_name (file_name_expanded)),
					level => log_threshold + 1);
			
			-- Create an empty module named after the module file (omitting extension *.mod).
			-- So the module names are things like "motor_driver", "templates/clock_generator" or
			-- "$TEMPLATES/clock_generator" or "/home/user/templates/clock_generator":
			pac_generic_modules.insert (
				container	=> generic_modules,
				key			=> to_module_name (remove_extension (file_name)),
				position	=> module_cursor,
				inserted	=> module_inserted);

			
			-- If the module is new to the collection of generic modules,
			-- then open the module file file and read it. 
			-- Otherwise notify operator that module has already been loaded.			 
			if module_inserted then

				read_module_file;
						
				-- Pointer module_cursor points to the last module that has been read.		
				-- The names of submodule/template files are stored in module.submods.file.
				-- But the files itself have not been read. That is what we do next:
				read_submodule_files;

				-- Test existence of assembly variants of submodules.
				test_assembly_variants_of_submodules (module_cursor, log_threshold + 1);
				
			else
				log (text => "module " & enclose_in_quotes (file_name) &
					" already loaded -> no need to load anew.", level => log_threshold + 1);
			end if;


			
		else -- module file not found
			raise semantic_error_1 with
				"ERROR: Module file " & enclose_in_quotes (file_name) & " not found !";
		end if;

		
		log_indentation_down;
		
		exception when event: others =>
			if is_open (file_handle) then close (file_handle); end if;
			set_input (previous_input);
			raise;
		
	end read_module;


	
	
	
end et_module_read;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
