------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / NET CLASSES                             --
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
--
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module_names;				use et_module_names;
with et_module_instance;
with et_keywords;					use et_keywords;

with et_text;
with et_schematic_text;

with et_schematic_geometry;			use et_schematic_geometry;
with et_schematic_coordinates;		use et_schematic_coordinates;
with et_sheets;						use et_sheets;

with et_symbol_ports;

with et_submodules;

with et_net_names;					use et_net_names;
with et_net_class_name;
with et_net_segment;				use et_net_segment;
with et_net_strands;				use et_net_strands;
with et_net_junction;				use et_net_junction;
with et_net_connectors;				use et_net_connectors;
with et_net_labels;					use et_net_labels;
with et_net_ports;
with et_nets;						use et_nets;
with et_netlists;
with et_general_rw;					use et_general_rw;



package body et_module_read_nets is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_net_name;

	
	
-- NETS:
		
	net_name	: pac_net_name.bounded_string; -- motor_on_off
	net			: et_nets.type_net;





	procedure read_net (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level)				   
	is
		kw : constant string := f (line, 1);
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






		

	procedure assign_net (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
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
		end query_module;

		
	begin
		log (text => "assign net", level => log_threshold);
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		-- clean up for next net
		net_name := to_net_name ("");
		net := (others => <>);
		
		log_indentation_down;
	end assign_net;





	



-- STRANDS:
		
	strands : pac_strands.list;
	strand	: type_strand;



	procedure read_strand (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
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






	procedure assign_net_strand (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
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


	

	procedure insert_strands is begin
		net.strands := strands;
		pac_strands.clear (strands); -- clean up for next strand collection
	end;

	
	
	
	

-- SEGMENTS:
		
	net_segments	: pac_net_segments.list;
	net_segment		: type_net_segment;

	
	
	procedure read_net_segment (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
		kw : constant string := f (line, 1);
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




	
	procedure assign_net_segment (
		log_threshold	: in type_log_level)
	is begin
		log (text => "assign net segment", level => log_threshold + 1);
		log_indentation_up;

		
		et_net_segment.pac_net_segments.append (
			container	=> net_segments,
			new_item	=> net_segment);

		-- Clean up for next net segment:
		reset_net_segment (net_segment);

		log_indentation_down;
	end assign_net_segment;

	
	
	
	
	
	
	procedure insert_net_segments is begin
		strand.segments := net_segments;

		-- clean up for next segment collection
		pac_net_segments.clear (net_segments);
	end;
	
	
	
	

	
-- JUNCTIONS:
		
	net_junctions	: et_net_junction.type_junctions;

	
	
	-- Reads a line that describes a net junction
	-- (like "A/B") that is active on an end
	-- of a net segment. Once the line is read, the junction
	-- is activated in variable net_junctions (see above):
	procedure read_net_junction (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
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


	
	


	procedure assign_net_junctions (
		log_threshold	: in type_log_level)
	is begin
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




	

	
	
	
-- CONNECTORS:

	net_connectors	: et_net_connectors.type_net_connectors;
	

	-- Reads a line that describes a net connector 
	-- (like "A/B direction input/output") that is attached to an end
	-- of a net segment. Once the line is read, the connector
	-- is assigned to the variable net_connectors (see above):
	procedure read_net_connector (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
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





	procedure assign_net_connectors (
		log_threshold	: in type_log_level)
	is begin
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


	
	
	
	
	
-- LABELS:
		
	net_labels			: pac_net_labels.list;
	net_label 			: type_net_label;
	
	net_label_rotation	: et_text.type_rotation_documentation := 
		et_text.type_rotation_documentation'first;

	-- The net label direction is relevant if it is a tag label:
	net_label_direction : type_connector_direction := 
		type_connector_direction'first;

	-- CS warn about parameter "direction" being ignored


	
	
	procedure read_label (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
		use et_schematic_text.pac_text_schematic;
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


	
	
	
	procedure insert_net_label is begin

		-- insert a simple label
		pac_net_labels.append (
			container	=> net_labels,
			new_item	=> net_label);

		-- clean up for next label
		net_label := (others => <>);
	end;
	

	
	

	procedure assign_net_labels (
		log_threshold	: in type_log_level)
	is begin
		log (text => "assign net connectors", level => log_threshold + 1);
		log_indentation_up;
		
		net_segment.labels := net_labels;

		-- clean up for next label collection
		pac_net_labels.clear (net_labels);

		log_indentation_down;
	end assign_net_labels;


	
	
	
	

	
	
	
-- PORTS:
	

	net_device_port : et_net_ports.type_device_port;
	-- net_device_ports : et_net_segment.pac_device_ports.set;

	net_submodule_port : et_net_ports.type_submodule_port;
	-- net_submodule_ports : et_net_segment.pac_submodule_ports.set;

	net_netchanger_port : et_netlists.type_port_netchanger;
	-- net_netchanger_ports : et_netlists.pac_netchanger_ports.set;

	net_segment_ports : et_net_ports.type_ports_AB;

	

	
	
	procedure read_net_port (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level)
	is
		use et_module_instance;
		use et_symbol_ports;
		use et_net_ports;
		
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

	
	

	
	

	procedure assign_net_ports (
		log_threshold	: in type_log_level)
	is begin
		log (text => "assign net ports", level => log_threshold + 1);
		log_indentation_up;
		
		net_segment.ports := net_segment_ports;
		
		-- clean up for next port collections (of another net segment)
		net_segment_ports := (others => <>);

		log_indentation_down;
	end assign_net_ports;

	
	
end et_module_read_nets;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
