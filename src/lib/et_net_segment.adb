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

with et_module_names;


package body et_net_segment is


	
	function "<" (left, right : in type_device_port) return boolean is
		use pac_port_name;
		use pac_unit_name;
	begin
		-- compare device names:
		if left.device_name < right.device_name then
			return true;
			
		elsif left.device_name = right.device_name then

			
			-- compare unit names:
			if left.unit_name < right.unit_name then
				return true;
				
			elsif left.unit_name = right.unit_name then


				-- compare port names:
				if left.port_name < right.port_name then
					return true;
				else
					return false;
				end if;

			else
				return false;
			end if;

			
		else
			return false;
		end if;
	end;


	
	
	function "<" (left, right : in type_submodule_port) return boolean is
		use et_module_names;
		use pac_module_instance_name;
		use et_net_names.pac_net_name;
	begin
		if left.module_name < right.module_name then
			return true;
		elsif left.module_name > right.module_name then
			return false;
		elsif left.port_name < right.port_name then
			return true;
		else
			return false;
		end if;
	end;




	
	
	function to_string (
		port : in type_device_port) 
		return string 
	is 
		use pac_unit_name;
	begin
		return "device " & to_string (port.device_name)
			& " unit " & to_string (port.unit_name)
			& " port " & to_string (port.port_name);
	end to_string;



	
	
	procedure iterate (
		ports	: in pac_device_ports.set;
		process	: not null access procedure (position : in pac_device_ports.cursor);
		proceed	: not null access boolean)
	is
		use pac_device_ports;
		c : pac_device_ports.cursor := ports.first;
	begin
		while c /= pac_device_ports.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



	

	function in_ports (
		ports	: in type_ports;
		port	: in et_netlists.type_port_netchanger)
		return boolean
	is
		result : boolean := false;

		use et_netlists;
		use pac_netchanger_ports;
		port_cursor : pac_netchanger_ports.cursor;
	begin
		port_cursor := ports.netchangers.find (port);
		
		if has_element (port_cursor) then
			result := true;
		end if;

		return result;
	end in_ports;





	function in_ports (
		ports	: in type_ports;
		port	: in type_submodule_port)
		return boolean
	is
		result : boolean := false;

		use pac_submodule_ports;
		port_cursor : pac_submodule_ports.cursor;
	begin
		port_cursor := ports.submodules.find (port);
		
		if has_element (port_cursor) then
			result := true;
		end if;

		return result;
	end in_ports;



	

	function no_ports (
		ports : in type_ports) 
		return boolean 
	is
		result : boolean := true;
		use pac_device_ports;
		use pac_submodule_ports;
		use et_netlists.pac_netchanger_ports;
	begin
		if length (ports.devices) > 0 then
			return false;
		end if;

		if length (ports.submodules) > 0 then
			result := false;
		end if;

		if length (ports.netchangers) > 0 then
			result := false;
		end if;

		return result;
	end no_ports;





	

	procedure junction_in_sloping_segment (
		point : in type_object_position) 
	is begin
		log (ERROR, "Junction not allowed in a sloping net segment at" 
			 & to_string (point),
			 console => true);
		raise constraint_error;
	end;

	

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
	


	
end et_net_segment;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
