------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                NETS                                      --
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

with et_conductor_segment.boards;
with et_fill_zones.boards;
with et_vias;
with et_module_names;


package body et_nets is


	function to_string (
		net_count : in type_net_count)
		return string
	is begin
		return type_net_count'image (net_count);
	end to_string;





	procedure set_proposed (
		net : in out type_net)
	is begin
		set_proposed (net.status);
	end;

	
	procedure clear_proposed (
		net : in out type_net)
	is begin
		clear_proposed (net.status);
	end;
		

	function is_proposed (
		net : in type_net)
		return boolean
	is begin
		if is_proposed (net.status) then
			return true;
		else
			return false;
		end if;
	end is_proposed;




	procedure set_selected (
		net : in out type_net)
	is begin
		set_selected (net.status);
	end;

	
	procedure clear_selected (
		net : in out type_net)
	is begin
		clear_selected (net.status);
	end;
		

	function is_selected (
		net : in type_net)
		return boolean
	is begin
		if is_selected (net.status) then
			return true;
		else
			return false;
		end if;
	end is_selected;


	
	

	procedure modify_status (
		net			: in out type_net;
		operation	: in type_status_operation)
	is begin
		modify_status (net.status, operation);
	end;
	




	procedure reset_status (
		net			: in out type_net)
	is begin
		reset_status (net.status);
	end;
	
	


	procedure create_strand (
		net		: in out type_net;
		sheet	: in type_sheet;						
		segment	: in type_net_segment)
	is 
		strand : type_strand; -- the new strand
	begin
		set_sheet (strand, sheet);
		
		-- Insert the given segment in the new strand:
		strand.segments.append (segment);

		-- Add the new strand to the given net:
		net.strands.append (strand);
	end create_strand;


	

	function has_strands (
		net : in type_net)
		return boolean
	is begin
		if net.strands.length > 0 then
			return true;
		else
			return false;
		end if;
	end;

	
	

	function get_strand (
		net		: in type_net;
		place	: in type_object_position)
		return pac_strands.cursor
	is
		result : pac_strands.cursor := pac_strands.no_element;

		proceed : aliased boolean := true;

		procedure query_strand (s : in pac_strands.cursor) is begin
			if on_strand (s, place) then
				proceed := false;
				result := s;
			end if;
		end query_strand;
		
	begin
		iterate (net.strands, query_strand'access, proceed'access);

		return result;
	end get_strand;


	
	


	function get_strands (
		net		: in type_net;
		sheet	: in type_sheet)
		return pac_strands.list
	is
		result : pac_strands.list;

		procedure query_strand (s : in pac_strands.cursor) is begin
			if get_sheet (s) = sheet then
				result.append (element (s));
			end if;
		end query_strand;
	
	begin
		iterate (net.strands, query_strand'access);

		return result;
	end get_strands;





	

	function get_strands (
		net		: in type_net;
		place	: in type_object_position)
		return pac_strand_cursors.list
	is
		result : pac_strand_cursors.list;

		strand_cursor : pac_strands.cursor := net.strands.first;
		
		
		procedure query_strand (strand : in type_strand) is 
			proceed : aliased boolean := true;
			
			procedure query_segment (segment : in pac_net_segments.cursor) is begin
				if on_segment (get_place (place), segment) then
					result.append (strand_cursor);
					proceed := false; -- no more probing required					
				end if;
			end query_segment;
			

		begin
			-- Look at strands which are on the given sheet:
			if get_sheet (strand) = get_sheet (place) then

				-- Iterate the segments of the strand:
				iterate (strand.segments, query_segment'access, proceed'access);
			end if;
		end query_strand;


	begin
		-- Iterate the strands of the net:
		while has_element (strand_cursor) loop
			query_element (strand_cursor, query_strand'access);
			next (strand_cursor);
		end loop;
		
		return result;
	end get_strands;

	


	

	function get_strands (
		net				: in type_net;
		primary			: in type_net_segment;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return pac_strand_segment_cursors.list
	is
		result : pac_strand_segment_cursors.list;

		
		strand_cursor : pac_strands.cursor := net.strands.first;
		
		
		procedure query_strand (strand : in type_strand) is
			segment_cursor : pac_net_segments.cursor := strand.segments.first;

			-- As soon as a segment has been found, then
			-- this flag is cleared so that no more segments
			-- are tested:
			proceed : boolean := true;
			
			-- Tests whether the candidate segment s ends
			-- either with its A end or its B end on the 
			-- given primary segment:
			procedure query_segment (s : in type_net_segment) is begin
				-- Test the A end of the candidate segment:
				if between_A_and_B (
					primary		=> primary, 
					AB_end		=> A,
					secondary	=> s)
				then
					log (text => "match on " & to_string (A) & " end", level => log_threshold + 1);
					
					-- Append the segment to the result:
					result.append ((strand_cursor, segment_cursor, A));
					proceed := false; -- no more probing required
				end if;

				-- Test the B end of the candidate segment:
				if between_A_and_B (
					primary		=> primary, 
					AB_end		=> B,
					secondary	=> s)
				then
					log (text => "match on " & to_string (B) & " end", level => log_threshold + 1);
					
					-- Append the segment to the result:
					result.append ((strand_cursor, segment_cursor, B));
					proceed := false; -- no more probing required
				end if;
			end query_segment;

				
		begin
			-- Iterate through the segments:
			while has_element (segment_cursor) and proceed loop
				log (text => "segment " & to_string (segment_cursor), level => log_threshold);
				log_indentation_up;
				query_element (segment_cursor, query_segment'access);
				log_indentation_down;
				next (segment_cursor);
			end loop;
		end query_strand;

		
	begin
		-- Iterate through the strands:
		while has_element (strand_cursor) loop
			
			-- We pick out only the strands on the given sheet:
			if get_sheet (strand_cursor) = sheet then
				query_element (strand_cursor, query_strand'access);					
			end if;
			
			next (strand_cursor);
		end loop;

		return result;
	end get_strands;





	
	

	procedure delete_strands (
		net		: in out type_net;
		strands	: in pac_strands.list)
	is
		c : pac_strands.cursor := strands.first;

		procedure query_strand (s : in pac_strands.cursor) is
			strand_to_delete : type_strand := element (s);
			cursor : pac_strands.cursor;
		begin
			cursor := find (net.strands, strand_to_delete);
			if cursor /= pac_strands.no_element then
				delete (net.strands, cursor);
			else
				put_line ("WARNING: Delete strands: Strand not found in net !");
				-- CS more details ?
			end if;
		end query_strand;
		
	begin
		iterate (strands, query_strand'access);
	end delete_strands;



	

	procedure add_strand (
		net		: in out type_net;
		strand	: in type_strand)
	is begin
		append (net.strands, strand);
	end add_strand;



	

	procedure add_strands (
		net		: in out type_net;
		strands	: in out pac_strands.list)
	is begin
		splice (
			target	=> net.strands,
			before	=> pac_strands.no_element,
			source	=> strands);
	end add_strands;



	
	

	procedure merge_strands (
		net				: in out type_net;
		target			: in pac_strands.cursor;
		source			: in out pac_strands.cursor;
		joint			: in type_strand_joint;
		log_threshold	: in type_log_level)
	is
		procedure query_strand (target : in out type_strand) is begin
			merge_strands (
				target			=> target, 
				source			=> element (source),
				joint			=> joint,
				log_threshold	=> log_threshold + 1);
		end query_strand;
		
	begin
		log (text => "merge strands", level => log_threshold);
		log_indentation_up;

		if joint.point then
			log (text => "join at point: " & to_string (joint.joint), level => log_threshold);
		else
			log (text => "join via segment: " & to_string (joint.segment), level => log_threshold);
		end if;
			
		-- Locate the target strand in the given net
		-- and merge it with the source strand:
		net.strands.update_element (target, query_strand'access);

		-- The source strand is no longer needed.
		-- Delete the source strand in the net.
		net.strands.delete (source);

		-- Cursor "source" now points to no_element.
		log_indentation_down;
	end merge_strands;

	

	

	procedure merge_nets (
		target	: in out type_net;
		source	: in out type_net)
	is
		use et_conductor_segment.boards;
		use pac_conductor_lines;
		use pac_conductor_arcs;

		use et_vias;
		use pac_vias;

		use et_fill_zones.boards;
		use pac_route_solid;
		use pac_route_hatched;
		use pac_cutouts;
		
	begin
		-- SCHEMATIC
		
		-- strands:
		splice (
			target => target.strands, 
			before => pac_strands.no_element,
			source => source.strands);
		-- CS: not good. use procedure merge_strands ?

		-- BOARD:
		
		-- conductor lines:
		splice (
			target => target.route.lines, 
			before => pac_conductor_lines.no_element,
			source => source.route.lines);

		-- conductor arcs:
		splice (
			target => target.route.arcs, 
			before => pac_conductor_arcs.no_element,
			source => source.route.arcs);

		-- vias:
		splice (
			target => target.route.vias, 
			before => pac_vias.no_element,
			source => source.route.vias);

		-- fill zones:
		splice (
			target => target.route.zones.solid, 
			before => pac_route_solid.no_element,
			source => source.route.zones.solid);
		
		splice (
			target => target.route.zones.hatched, 
			before => pac_route_hatched.no_element,
			source => source.route.zones.hatched);

		-- cutout areas:
		-- CS now net specific restrict stuff
		--splice (
			--target => target.route.cutouts, 
			--before => pac_cutouts.no_element,
			--source => source.route.cutouts);

		
	end merge_nets;





	function has_end_point (
		net		: in type_net;
		place	: in type_object_position)
		return boolean
	is
		-- If the given net has no segment that starts
		-- or ends at the given place, then this flag remains set.
		-- So the negated value will be returned to the caller of
		-- this function:
		proceed : aliased boolean := true;

		-- Extract sheet number and location vector from
		-- the given place:
		sheet : constant type_sheet := get_sheet (place);
		point : constant type_vector_model := get_place (place);
		

		procedure query_strand (s : in pac_strands.cursor) is
			strand : type_strand renames element (s);
			all_points_of_strand : pac_points.list;
		begin
			-- Look at strands that are on the given sheet only:
			if get_sheet (strand) = sheet then

				-- Get all end points of the candidate strand:
				all_points_of_strand := get_end_points (strand.segments);

				-- If the given point is among the points
				-- of the strand, then abort the iteration of strands:
				if all_points_of_strand.contains (point) then
		
					-- No more probing required:
					proceed := false;
				end if;
				
			end if;
		end query_strand;
		
	begin
		-- Iterate the strands of the given net. Abort on the first
		-- strand that has a matching segment.
		-- By the way, only one strand can exist at the given place anyway:
		iterate (net.strands, query_strand'access, proceed'access);

		return not proceed;
	end has_end_point;

	
	

	


	function is_proposed (
		net : in pac_nets.cursor)
		return boolean
	is begin
		return is_proposed (element (net));
	end;


	function is_selected (
		net : in pac_nets.cursor)
		return boolean
	is begin
		return is_selected (element (net));
	end;





	

	function get_net_name (
		net_cursor : in pac_nets.cursor)
		return pac_net_name.bounded_string
	is begin
		return key (net_cursor);
	end;


	

	
	function get_net_name (
		net_cursor : in pac_nets.cursor)
		return string
	is begin
		return to_string (key (net_cursor));
	end get_net_name;


	


	function net_exists (
		net_cursor : in pac_nets.cursor) 
		return boolean 
	is begin
		if net_cursor = pac_nets.no_element then
			return false;
		else 
			return true;
		end if;
	end;


	
	

	procedure iterate (
		nets	: in pac_nets.map;
		process	: not null access procedure (position : in pac_nets.cursor);
		proceed	: not null access boolean)
	is
		c : pac_nets.cursor := nets.first;
	begin
		while c /= pac_nets.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;





	

	function get_ports (
		net		: in pac_nets.cursor;
		variant	: in pac_assembly_variants.cursor := pac_assembly_variants.no_element)
		return type_ports 
	is
		result : type_ports; -- to be returned

		
		procedure query_segments (segment_cursor : in pac_net_segments.cursor) is
			use pac_device_ports;

			use et_netlists;
			use pac_netchanger_ports;			
			use pac_submodule_ports;

			
			-- Inserts the device/port in result.devices. Skips the device/port
			-- according to the given assembly variant.
			procedure query_devices (device_cursor : in pac_device_ports.cursor) is begin
				if et_assembly_variants.is_mounted (
					device		=> element (device_cursor).device_name, -- IC4, R101
					variant		=> variant) 
				then
					--put_line (to_string (element (device_cursor)));
					
					insert (
						container	=> result.devices,
						new_item	=> element (device_cursor));
				end if;

				exception
					when event: others =>
						raise constraint_error with to_string (element (device_cursor))
						--put_line (to_string (element (device_cursor))
						& " already in set !";
						
			end query_devices;

			
		begin
			-- Collect device ports of segment according to given assembly variant.
			iterate (element (segment_cursor).ports.A.devices, query_devices'access);
			iterate (element (segment_cursor).ports.B.devices, query_devices'access);

			-- Ports of netchangers and submodules go into the result right away
			-- because they are not affected by any assembly variants.
			union (result.netchangers, element (segment_cursor).ports.A.netchangers);
			union (result.netchangers, element (segment_cursor).ports.B.netchangers);
			
			union (result.submodules, element (segment_cursor).ports.A.submodules);
			union (result.submodules, element (segment_cursor).ports.B.submodules);
		end query_segments;

		
		
		procedure query_strands (strand_cursor : in pac_strands.cursor) is begin
			iterate (element (strand_cursor).segments, query_segments'access);
		end query_strands;

		
	begin
		--put_line ("net " & to_string (key (net)));		
		iterate (element (net).strands, query_strands'access);
		return result;
	end get_ports;


	



	function has_strands (
		net : in pac_nets.cursor)
		return boolean
	is begin
		return has_strands (element (net));
	end;
	

	

	function get_first_strand (
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor
	is
		strand_cursor : pac_strands.cursor; -- to be returned
		strand_position : type_object_position := greatest_position;

		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is

			procedure query_strand (c : in pac_strands.cursor) is begin
				if element (c).position < strand_position then
					strand_position := element (c).position;
					strand_cursor := c;
				end if;
			end query_strand;
			
		begin			
			iterate (net.strands, query_strand'access);
		end query_strands;

		
	begin -- get_first_strand
		query_element (
			position	=> net_cursor,
			process		=> query_strands'access);
	
		return strand_cursor;
	end get_first_strand;




	

	function get_first_strand_on_sheet (
		sheet		: in type_sheet;
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor
	is
		strand_cursor : pac_strands.cursor; -- to be returned

		strand_position : type_object_position := greatest_position;

		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is
			c : pac_strands.cursor := net.strands.first;
		begin			
			while c /= pac_strands.no_element loop

				-- Probe strands on the given sheet only:
				if get_sheet (element (c).position) = sheet then

					if element (c).position < strand_position then
						strand_position := element (c).position;
						strand_cursor := c;
					end if;

				end if;
				
				next (c); -- advance to next strand
			end loop;
		end query_strands;

		
	begin
		query_element (
			position	=> net_cursor,
			process		=> query_strands'access);

		return strand_cursor;
	end get_first_strand_on_sheet;
	




	



	function to_string (
		object	: in type_object_segment)
		return string
	is begin
		return "net " & get_net_name (object.net_cursor) 
			-- & " strand " & get_position (object.strand_cursor)
			& " segment " & to_string (object.segment_cursor);
	end;



	function get_sheet (
		object	: in type_object_segment)
		return type_sheet
	is 
		segment : type_net_segment;
	begin
		return get_sheet (object.strand_cursor);
	end;







	function is_empty (
		strand	: in type_object_strand)
		return boolean
	is begin
		if not has_element (strand.net_cursor) 
		and not has_element (strand.strand_cursor) 
		then
			return true;
		else
			return false;
		end if;
	end is_empty;
	
	



	function get_net_name (
		strand	: in type_object_strand)
		return pac_net_name.bounded_string
	is begin
		return key (strand.net_cursor);
	end;

	


	function to_string (
		object	: in type_object_strand)
		return string
	is begin
		return "net " & get_net_name (object.net_cursor) 
			& " strand " & get_position (object.strand_cursor);
	end to_string;





	function get_strand (
		strand : in type_object_strand)
		return type_strand
	is begin
		return element (strand.strand_cursor);
	end get_strand;

	



	function get_net_name (
		strand	: in pac_object_strands.cursor)
		return pac_net_name.bounded_string
	is 
		use pac_object_strands;
		s : type_object_strand := element (strand);
	begin
		return key (s.net_cursor);
	end;
	


	
	

	function get_strand (
		strands			: in pac_object_strands.list;
		net_cursor		: in pac_nets.cursor)
		return type_object_strand
	is 
		result : type_object_strand;

		proceed : boolean := true;

		-- Query a single object strand and compare
		-- its net name with the give net name.
		-- When both match, clear the proceed flag so that
		-- the iteration stops.
		-- Save the object strand in the return value:
		procedure query_strand (s : in type_object_strand) is begin
			if get_net_name (s) = key (net_cursor) then
				proceed := false;

				result := s;
			end if;
		end query_strand;

		
		use pac_object_strands;
		c : pac_object_strands.cursor := strands.first;
		
	begin
		-- Iterate the given strands. Abort when the proceed-flag is set:
		while has_element (c) loop
			query_element (c, query_strand'access);
			
			if not proceed then
				exit;
			end if;
			
			next (c);
		end loop;
		
		return result;
	end get_strand;






	


	function to_string (
		object	: in type_object_net)
		return string
	is begin
		return "net " & get_net_name (object.net_cursor);
	end to_string;




	function to_string (
		object	: in type_object_label)
		return string
	is begin
		return "net " & get_net_name (object.net_cursor)
			& " simple label at " & get_position (object.label_cursor);
			-- CS other properties ?
	end;

	
	
	function to_string (
		object	: in type_object_label_tag)
		return string
	is begin
		return "net " & get_net_name (object.net_cursor)
			& " tag label"; 
		-- CS Strand " & get_position (object.strand_cursor)
		-- & " segment " & to_string (object.segment_cursor)
		-- & to_string (object.start_end) & " point";
		-- CS direction, rotation
	end;

	
	
end et_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
