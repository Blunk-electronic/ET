------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             NET SEGMENT                                  --
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
-- <http://www.gnu.org/licenses/>.   
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


package body et_net_segment is
	

	function to_net_segment (
		A, B : in type_vector_model)
		return type_net_segment
	is 
		segment : type_net_segment;
	begin
		set_A (segment, A);
		set_B (segment, B);
		return segment;
	end to_net_segment;





	procedure set_junction (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point)
	is begin
		case AB_end is
			when A => segment.junctions.A := true;
			when B => segment.junctions.B := true;
		end case;
	end;

	

	procedure clear_junction (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point)
	is begin
		case AB_end is
			when A => segment.junctions.A := false;
			when B => segment.junctions.B := false;
		end case;
	end;




	function get_junction_status (
		segment	: in type_net_segment;
		AB_end	: in type_start_end_point)
		return boolean
	is begin
		case AB_end is
			when A => return segment.junctions.A;
			when B => return segment.junctions.B;
		end case;
	end;


	

	function get_junction_status (
		segment		: in type_net_segment;
		NSWE_end	: in type_direction_NSWE)
		return boolean
	is
		result : boolean;
		AB_end : type_start_end_point;
	begin
		-- Map from the given NSWE end to the AB end:
		AB_end := get_NSWE_end (segment, NSWE_end);

		-- Get the junction statuss on the AB end:
		result := get_junction_status (segment, AB_end);
		return result;
	end get_junction_status;
	


	
	

	function is_connected (
		segment	: in type_net_segment;
		port	: in et_netlists.type_port_netchanger)
		return boolean
	is
	begin
		if 	in_ports (segment.ports.A, port) or 
			in_ports (segment.ports.B, port) then

			return true;
		else
			return false;
		end if;
	end is_connected;




	procedure insert_netchanger_port (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point;
		port	: in et_netlists.type_port_netchanger)
	is
		use et_netlists;
		position : pac_netchanger_ports.cursor;
		inserted : boolean;
	begin
		case AB_end is
			when A =>
				segment.ports.A.netchangers.insert (port, position, inserted);

			when B =>
				segment.ports.B.netchangers.insert (port, position, inserted);
		end case;
	end insert_netchanger_port;


	

	procedure insert_submodule_port (
		segment	: in out type_net_segment;
		AB_end	: in type_start_end_point;
		port	: in type_submodule_port)
	is 
		position : pac_submodule_ports.cursor;
		inserted : boolean;
	begin
		case AB_end is
			when A =>
				segment.ports.A.submodules.insert (port, position, inserted);

			when B =>
				segment.ports.B.submodules.insert (port, position, inserted);
		end case;
	end insert_submodule_port;






	procedure delete_netchanger_port (
		segment	: in out type_net_segment;
		port	: in et_netlists.type_port_netchanger;
		deleted : out boolean)
	is
		use et_netlists;
		use pac_netchanger_ports;
		cursor : pac_netchanger_ports.cursor;
	begin
		deleted := false;

		
		-- Search port at A end:
		cursor := find (segment.ports.A.netchangers, port);

		if has_element (cursor) then
			segment.ports.A.netchangers.delete (cursor);
			deleted := true;
		end if;


		-- If not found or not deleted then search at B end:
		if not deleted then
			cursor := find (segment.ports.B.netchangers, port);

			if has_element (cursor) then
				segment.ports.B.netchangers.delete (cursor);
				deleted := true;
			end if;
		end if;
	end delete_netchanger_port;

	
	

	procedure delete_submodule_port (
		segment	: in out type_net_segment;
		port	: in type_submodule_port;
		deleted : out boolean)
	is
		use pac_submodule_ports;
		cursor : pac_submodule_ports.cursor;
	begin
		deleted := false;

		
		-- Search port at A end:
		cursor := find (segment.ports.A.submodules, port);

		if has_element (cursor) then
			segment.ports.A.submodules.delete (cursor);
			deleted := true;
		end if;


		-- If not found or not deleted then search at B end:
		if not deleted then
			cursor := find (segment.ports.B.submodules, port);

			if has_element (cursor) then
				segment.ports.B.submodules.delete (cursor);
				deleted := true;
			end if;
		end if;
	end delete_submodule_port;



	

	procedure delete_submodule_ports (
		segment	: in out type_net_segment;
		module	: in pac_module_instance_name.bounded_string)
	is
		use pac_submodule_ports;
		port_cursor : pac_submodule_ports.cursor;
		port : type_submodule_port;

		use pac_module_instance_name;
	begin
		-- Delete at A end:
		port_cursor := segment.ports.A.submodules.first;

		while has_element (port_cursor) loop
			port := element (port_cursor);
			
			if port.module_name = module then
				segment.ports.A.submodules.delete (port);
			end if;
			next (port_cursor);
		end loop;


		-- Delete at B end:
		port_cursor := segment.ports.B.submodules.first;

		while has_element (port_cursor) loop
			port := element (port_cursor);
			
			if port.module_name = module then
				segment.ports.B.submodules.delete (port);
			end if;
			next (port_cursor);
		end loop;		
	end delete_submodule_ports;




	
	function has_ports (
		segment : in type_net_segment;
		AB_end	: in type_start_end_point)				   
		return boolean
	is 
		count : count_type := 0;
	begin
		case AB_end is
			when A =>
				count := segment.ports.A.devices.length;
				count := count + segment.ports.A.netchangers.length;
				count := count + segment.ports.A.submodules.length;

			when B =>
				count := segment.ports.B.devices.length;
				count := count + segment.ports.B.netchangers.length;
				count := count + segment.ports.B.submodules.length;
		end case;

		if count = 0 then
			return false;
		else
			return true;
		end if;
	end has_ports;

	
				
	
	function has_ports (
		segment : in type_net_segment)
		return boolean
	is 
		count : count_type := 0;
	begin
		count := segment.ports.A.devices.length;
		count := count + segment.ports.B.devices.length;

		count := count + segment.ports.A.netchangers.length;
		count := count + segment.ports.B.netchangers.length;

		count := count + segment.ports.A.submodules.length;
		count := count + segment.ports.B.submodules.length;
		
		if count = 0 then
			return false;
		else
			return true;
		end if;
	end has_ports;




	function get_ports (
		segment : in type_net_segment;
		AB_end	: in type_start_end_point)				   
		return type_ports
	is begin
		case AB_end is
			when A => return segment.ports.A;
			when B => return segment.ports.B;
		end case;
	end;

	


	function get_ports (
		segment 	: in type_net_segment;
		NSWE_end	: in type_direction_NSWE)				   
		return type_ports
	is
		result : type_ports;
		AB_end : type_start_end_point;
	begin
		-- Map from the given NSWE end to the AB end:
		AB_end := get_NSWE_end (segment, NSWE_end);

		-- Get the ports on the AB end:
		result := get_ports (segment, AB_end);
		return result;
	end get_ports;
	

	

	

	function get_port_count (
		segment : in type_net_segment;
		AB_end	: in type_start_end_point)				   
		return natural
	is 
		ports : type_ports;
		count : count_type := 0;
	begin
		ports := get_ports (segment, AB_end);

		count := ports.devices.length;
		count := count + ports.netchangers.length;
		count := count + ports.submodules.length;

		return natural (count);		
	end;


	

	procedure append_ports (
		segment : in out type_net_segment;
		ports	: in type_ports;						   
		AB_end	: in type_start_end_point)
	is
		use pac_device_ports;
		use pac_submodule_ports;
		use et_netlists.pac_netchanger_ports;
	begin
		case AB_end is
			when A =>
				segment.ports.A.devices.union (ports.devices);
				segment.ports.A.submodules.union (ports.submodules);
				segment.ports.A.netchangers.union (ports.netchangers);

			when B =>
				segment.ports.B.devices.union (ports.devices);
				segment.ports.B.submodules.union (ports.submodules);
				segment.ports.B.netchangers.union (ports.netchangers);
		end case;
	end append_ports;


	
	

	function split_segment (
		segment	: in type_net_segment;
		point	: in type_vector_model)
		return type_split_segment
	is
		-- Prepare return for two cases:

		-- CASE 1: No split because point is on A or B end of segment:
		result_no_split	: type_split_segment (count => 1);

		-- CASE 2: The segment will be split in two fragments:
		result_split	: type_split_segment (count => 2);

		-- Treat the segment as a regular line and split it.
		-- Depending on the result we decide how to proceed further:
		fragments : type_split_line := split_line (segment, point);

		
		procedure do_it is
			segment_1, segment_2 : type_net_segment;
		begin
			segment_1 := (fragments.segments (1) with others => <>);
			segment_1.ports.A := segment.ports.A;
			segment_1.junctions.A := segment.junctions.A;
			segment_1.tag_labels.A := segment.tag_labels.A;
			-- CS labels. Currently simple labels are discarded.
			
			segment_2 := (fragments.segments (2) with others => <>);
			segment_2.ports.B := segment.ports.B;
			segment_2.junctions.B := segment.junctions.B;
			segment_2.tag_labels.B := segment.tag_labels.B;
			-- CS labels. Currently simple labels are discarded.

			result_split.segments (1) := segment_1;
			result_split.segments (2) := segment_2;
		end do_it;

		
	begin
		-- Evaluate the fragments:
		case fragments.count is
			when 1 =>
				-- No splitting because point sits 
				-- on A or B of given segment. The given
				-- segment is returned as it is:
				result_no_split.segments (1) := segment;
				return result_no_split;

			when 2 =>
				do_it;
				return result_split;

			when others => raise constraint_error; -- CS should never happen
		end case;
	end split_segment;



	
	

	procedure merge_segments (
		primary			: in out type_net_segment;
		primary_end		: in type_start_end_point;
		secondary		: in type_net_segment;
		secondary_end	: in type_start_end_point)
	is 
		labels_primary, labels_secondary : pac_net_labels.list;
		-- CS labels_primary_tag, labels_secondary_tag : type_tag_labels;
		
		ports : type_ports_AB;
		junctions : type_junctions;
	begin
		-- The ends where the segments are to be merged
		-- must not have ports. As a safety measure this
		-- check is required:
		if has_ports (primary, primary_end) then
			raise constraint_error;
		end if;
		
		if has_ports (secondary, secondary_end) then
			raise constraint_error;
		end if;


		-- Backup the ports at the open ends of the two segments:
		ports.A := get_ports (primary,   get_opposide_end (primary_end));
		ports.B := get_ports (secondary, get_opposide_end (secondary_end));

		-- Backup the junction status at the open ends of the two segments:
		junctions.A := get_junction_status (primary,   get_opposide_end (primary_end));
		junctions.B := get_junction_status (secondary, get_opposide_end (secondary_end));
		
		-- Merge the simple labels of the two segments:
		labels_primary   := primary.labels;
		labels_secondary := secondary.labels;
		merge_labels (labels_primary, labels_secondary);

		
		-- Merge the segments:
		merge_lines (primary, primary_end, secondary, secondary_end);

		-- Assign the ports at the open ends of the resulting segment:
		primary.ports := ports;

		-- Assign the junction status to the resulting segment:
		primary.junctions := junctions;
		
		-- Assign the simple labels to the resulting segment:
		primary.labels := labels_primary;

	end merge_segments;



	
	
	procedure reset_status (
		segment : in out type_net_segment)
	is begin	
		pac_geometry_2.reset_status (line => type_line (segment));

		-- CS reset junction and labels
	end reset_status;



	


	function get_connect_status (
		primary 	: in type_net_segment;
		AB_end		: in type_start_end_point;
		secondary	: in type_net_segment)
		return type_connect_status
	is
		result : type_connect_status;

		-- The point where the test will take place:
		reference : type_vector_model;
	begin
		-- Set the reference point according to which
		-- end of the primary segment will be tested:
		case AB_end is
			when A => reference := get_A (primary);
			when B => reference := get_B (primary);
		end case;

		-- Test A end of secondary segment:
		if reference = get_A (secondary) then
			-- A end of secondary segment is connected with primary segment:
			result := CON_STS_A;

			
		-- Test B end of secondary segment:
		elsif reference = get_B (secondary) then
			-- B end of secondary segment is connected with primary segment:
			result := CON_STS_B;

		else
			-- Secondary segment is NOT connected with primary segment:
			result := CON_STS_NONE;
		end if;

		return result;
	end get_connect_status;






	function between_A_and_B (
		secondary	: in type_net_segment;
		AB_end		: in type_start_end_point;
		primary		: in type_net_segment)
		return boolean
	is
		point : type_vector_model;
	begin		
		case AB_end is
			when A => point := get_A (secondary);
			when B => point := get_B (secondary);
		end case;

		return between_A_and_B (primary, point);
	end between_A_and_B;





	function merge_segments (
		primary, secondary : in type_net_segment)
		return type_net_segment
	is
		result : type_net_segment;

		-- Get the orientation of the two lines.
		-- Both should be equal and none of the should be sloping:
		OP : constant type_line_orientation := get_orientation (primary);
		OS : constant type_line_orientation := get_orientation (secondary);



		procedure precheck is begin
			-- If any of the given lines is a slope then
			-- raise an exception:
			if OP = ORIENT_SLOPING or OS = ORIENT_SLOPING then
				raise constraint_error;
			end if;

			-- If the orientation of the lines differs, then
			-- raise an exception:
			if OP /= OS then
				raise constraint_error;
			end if;
		end precheck;

		
		

		-- Ports at A and B end of the resulting segment:
		PRA, PRB : type_ports;


		procedure merge_ports is 
			-- Ports at the A and B end of the primary segment:
			PPA, PPB : type_ports;
			
			-- Ports at the A and B end of the secondary segment:
			PSA, PSB : type_ports;
		begin
			-- The orientation determines whether to collect
			-- the ports from the west and east ends or
			-- from the south and north ends:
			case OP is
				when ORIENT_HORIZONTAL =>

					-- Collect the ports from the west ends:
					PPA := get_ports (primary,   DIR_WEST);
					PSA := get_ports (secondary, DIR_WEST);
					
					-- Collect the ports from the east ends:
					PPB := get_ports (primary,   DIR_EAST);
					PSB := get_ports (secondary, DIR_EAST);
					
					
				when ORIENT_VERTICAL =>
					-- Collect the ports from the south ends:
					PPA := get_ports (primary,   DIR_SOUTH);
					PSA := get_ports (secondary, DIR_SOUTH);

					-- Collect the ports from the north ends:
					PPB := get_ports (primary,  DIR_NORTH);
					PSB := get_ports (secondary, DIR_NORTH);

					
				when ORIENT_SLOPING =>
					raise constraint_error; -- CS should never happen
			end case;


			-- Union the ports on the A end:
			PRA := merge_ports (PPA, PSA);

			-- Union the ports on the B end:
			PRB := merge_ports (PPB, PSB);	
			
		end merge_ports;


		
		
		-- Junction status at A and B end of the resulting segment:
		JRA, JRB : boolean;

		
		procedure merge_junctions is		
			-- Junction status at the A and B end of the primary segment;
			JPA, JPB : boolean;

			-- Junction status at the A and B end of the secondary segment;
			JSA, JSB : boolean;
		begin
			-- The orientation determines whether to collect
			-- the ports from the west and east ends or
			-- from the south and north ends:
			case OP is
				when ORIENT_HORIZONTAL =>
					-- Get the junction status from the west ends:
					JPA := get_junction_status (primary,   DIR_WEST);
					JSA := get_junction_status (secondary, DIR_WEST);

					-- Get the junction status from the east ends:
					JPB := get_junction_status (primary,   DIR_EAST);
					JSB := get_junction_status (secondary, DIR_EAST);

					
					
				when ORIENT_VERTICAL =>
					-- Get the junction status from the south ends:
					JPA := get_junction_status (primary,   DIR_SOUTH);
					JSA := get_junction_status (secondary, DIR_SOUTH);

					-- Get the junction status from the north ends:
					JPB := get_junction_status (primary,   DIR_NORTH);
					JSB := get_junction_status (secondary, DIR_NORTH);

					
				when ORIENT_SLOPING =>
					raise constraint_error; -- CS should never happen
			end case;

			-- Union the junctions on the A end:
			JRA := JPA or JSA;

			-- Union the junctions on the B end:
			JRB := JPB or JSB;
			
		end merge_junctions;


		
		-- The simple labels of the resulting segment:
		LR : pac_net_labels.list;

		procedure merge_simple_labels is 
			use pac_net_labels;
			primary_labels : pac_net_labels.list := primary.labels;
			secondary_labels : pac_net_labels.list := secondary.labels;
		begin
			splice (
				target		=> primary_labels,
				before		=> pac_net_labels.no_element,
				source		=> secondary_labels);

			LR := primary_labels;
		end merge_simple_labels;
			
		
		line : type_line;
		
	begin
		precheck;
		
		merge_ports;

		merge_junctions;

		merge_simple_labels;
		
		
		line := type_line (merge_lines (primary, secondary));

		result := (line with 
				   ports => (PRA, PRB), 
				   junctions => (JRA, JRB), 
				   labels	=> LR,
				   others => <>);

		return result;
	end merge_segments;

	
	
	
	
-- 	procedure move_net_labels (
-- 		segment_before	: in type_net_segment;
-- 		segment_after	: in out type_net_segment;
-- 		zone			: in type_line_zone)
-- 	is 
-- 		-- Calculate the displacement of the start and end point:
-- 		
-- 		delta_start : constant type_vector_model :=
-- 			get_distance_relative (get_A (segment_before), get_A (segment_after));
-- 		
-- 		delta_end	: constant type_vector_model :=
-- 			get_distance_relative (get_B (segment_before), get_B (segment_after));
-- 															
-- 		use pac_net_labels;
-- 		label_cursor : pac_net_labels.cursor := segment_after.labels.first;
-- 
-- 		
-- 		procedure move (l : in out type_net_label) is begin
-- 			-- The position of a net label is absolute.
-- 			
-- 			case l.appearance is
-- 				when TAG => 
-- 					-- Moving the tag labels is quite simple because
-- 					-- they are always at start or end point.
-- 					-- So the label position change is just the displacement
-- 					-- of the start or end point:
-- 					case zone is
-- 						when START_POINT =>
-- 							if l.position = get_A (segment_before) then
-- 								move_by (l.position, delta_start);
-- 							end if;
-- 							
-- 						when END_POINT => 
-- 							if l.position = get_B (segment_before) then
-- 								move_by (l.position, delta_end);
-- 							end if;
-- 
-- 						when CENTER =>
-- 							if l.position = get_A (segment_before) then
-- 								move_by (l.position, delta_start);
-- 							end if;
-- 
-- 							if l.position = get_B (segment_before) then
-- 								move_by (l.position, delta_end);
-- 							end if;
-- 							
-- 					end case;
-- 
-- 					-- CS: change rotation of label ?
-- 					
-- 				when SIMPLE => null; -- CS
-- 					-- This requires a bit more math because simple labels
-- 					-- are mostly between start and end point.
-- 
-- 					-- CS: change rotation of label ?
-- 			end case;
-- 		end move;
-- 
-- 		
-- 	begin
-- 		while label_cursor /= pac_net_labels.no_element loop
-- 			update_element (segment_after.labels, label_cursor, move'access);
-- 			next (label_cursor);
-- 		end loop;
-- 		
-- 	end move_net_labels;



	function has_ports (
		segment : in pac_net_segments.cursor)
		return boolean
	is 
		s : type_net_segment := element (segment);
	begin
		return has_ports (s);
	end has_ports;




	function has_ports (
		segment : in pac_net_segments.cursor;
		AB_end	: in type_start_end_point)				   
		return boolean
	is
		s : type_net_segment := element (segment);
	begin
		return has_ports (s, AB_end);
	end;


	
	

	function is_selected (
		segment : in pac_net_segments.cursor)
		return boolean
	is begin
		if is_selected (element (segment)) then
			return true;
		else
			return false;
		end if;
	end is_selected;

	


	function get_A (
		segment : in pac_net_segments.cursor)
		return type_vector_model
	is begin
		return get_A (element (segment));
	end;



	function get_B (
		segment : in pac_net_segments.cursor)
		return type_vector_model
	is begin
		return get_B (element (segment));
	end;



	function get_end_point (
		segment : in pac_net_segments.cursor;
		AB_end	: in type_start_end_point)				   
		return type_vector_model
	is begin
		case AB_end is
			when A => return get_A (segment);
			when B => return get_B (segment);
		end case;
	end;

	
	
	
	function is_moving (
		segment : in pac_net_segments.cursor)
		return boolean
	is begin
		return is_moving (element (segment));
	end;


	

	function is_A_moving (
		segment	: in pac_net_segments.cursor)
		return boolean
	is begin
		return is_A_moving (element (segment));
	end;

	

	function is_B_moving (
		segment	: in pac_net_segments.cursor)
		return boolean
	is begin
		return is_B_moving (element (segment));
	end;
	

	
	procedure iterate (
		segments	: in pac_net_segments.list;
		process		: not null access procedure (position : in pac_net_segments.cursor);
		proceed		: not null access boolean)
	is
		c : pac_net_segments.cursor := segments.first;
	begin
		while c /= pac_net_segments.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	

	function to_string (
		segment : in pac_net_segments.cursor) 
		return string 
	is begin
		return to_string (element (segment));
	end to_string;





	
	
	function between_A_and_B (
		catch_zone	: in type_catch_zone;
		segment 	: in pac_net_segments.cursor)
		return boolean 
	is
		use pac_geometry_sch;
		dist : type_distance_point_line;
	begin
		dist := get_distance (
			point 		=> get_center (catch_zone),
			line		=> element (segment),
			line_range	=> BETWEEN_END_POINTS);

		if (not out_of_range (dist)) 
		and in_radius (get_distance (dist), get_radius (catch_zone)) then
			return true;
		else
			return false;
		end if;
	end between_A_and_B;
	







	
	
	function get_segment_orientation (
		segment : in pac_net_segments.cursor) 
		return type_line_orientation 
	is		
		S : type_net_segment := element (segment);
	begin
		return get_orientation (S);
	end get_segment_orientation;





	function segments_overlap (
		s1, s2 : in pac_net_segments.cursor)
		return boolean
	is begin
		return lines_overlap (element (s1), element (s2));
	end;





	

	
	function on_segment (
		catch_zone	: in type_catch_zone;
		segment 	: in pac_net_segments.cursor)
		return boolean 
	is
		use pac_geometry_sch;
		dist : type_distance_point_line;
	begin
		dist := get_distance (
			point 		=> get_center (catch_zone),
			line		=> element (segment),
			line_range	=> WITH_END_POINTS);

-- 		log (text => 
-- 			"catch zone" & to_string (catch_zone) 
-- 			& " distance " & to_string (distance (dist))
-- 			& " out of range " & boolean'image (out_of_range (dist))
-- 			);
		
		if (not out_of_range (dist)) 
		and in_radius (get_distance (dist), get_radius (catch_zone)) then
			return true;
		else
			return false;
		end if;
	end on_segment;




	function on_segment (
		point		: in type_vector_model;
		segment 	: in pac_net_segments.cursor)
		return boolean
	is 
		s : type_net_segment := element (segment);
	begin
		return on_line (s, point);
	end on_segment;
	




	function split_segment (
		segment	: in pac_net_segments.cursor;
		point	: in type_vector_model)
		return type_split_segment
	is begin		
		return split_segment (element (segment), point);
	end;

	

	
	
	function get_connect_status (
		primary 	: in pac_net_segments.cursor;
		AB_end		: in type_start_end_point;
		secondary	: in pac_net_segments.cursor)
		return type_connect_status
	is 
		result : type_connect_status;
		
		P : type_net_segment := element (primary);
		S : type_net_segment := element (secondary);
	begin
		result := get_connect_status (P, AB_end, S);
		return result;
	end get_connect_status;
	




	function get_end_points (
		segments	: in pac_net_segments.list)
		return pac_points.list
	is
		result : pac_points.list;

		
		procedure query_segment (c : in pac_net_segments.cursor) is
			A, B : type_vector_model;
		begin
			-- Append the A end of the candidate segment
			-- to the result if it has not already been appended:
			A := get_A (c);

			if not result.contains (A) then
				result.append (A);
			end if;
			
			-- Append the B end of the candidate segment
			-- to the result if it has not already been appended:
			B := get_B (c);

			if not result.contains (B) then
				result.append (B);
			end if;
		end query_segment;

		
	begin
		segments.iterate (query_segment'access);
		return result;
	end get_end_points;

	

	
	
end et_net_segment;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
