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


	


	function get_sheet (
		strand	: in type_strand)
		return type_sheet
	is begin
		return get_sheet (strand.position);
	end get_sheet;

	


	
	procedure iterate (
		strands	: in pac_strands.list;
		process	: not null access procedure (position : in pac_strands.cursor);
		proceed	: not null access boolean)
	is
		c : pac_strands.cursor := strands.first;
	begin
		while c /= pac_strands.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



	
	
	procedure set_strand_position (
		strand : in out type_strand) 
	is
		point_1, point_2 : type_vector_model;
	
		-- CS: usage of intermediate variables for x/Y of start/end points could improve performance

		procedure query_strand (cursor : in pac_net_segments.cursor) is begin
			-- Test start point of segment. 
			-- if closer to orign than point_1 keep start point
			point_2	:= get_A (cursor);
			if get_distance_absolute (point_2, origin) < get_distance_absolute (point_1, origin) then
				point_1 := point_2;
			end if;

			-- Test start point of segment.
			-- if closer to orign than point_1 keep end point
			point_2	:= get_B (cursor);
			if get_distance_absolute (point_2, origin) < get_distance_absolute (point_1, origin) then
				point_1 := point_2;
			end if;
		end query_strand;

		
	begin
		--log (text => "set strand position");
		
		-- init point_1 as the farest possible point from drawing origin
		point_1 := type_vector_model (set (
					x => type_position_axis'last,
					y => type_position_axis'last));

		-- loop through segments and keep the nearest point to origin
		iterate (strand.segments, query_strand'access);

		-- build and assign the final strand position from point_1
		strand.position.set (point_1);
		
	end set_strand_position;




	
	function get_first_segment (
		strand_cursor	: in pac_strands.cursor)
		return pac_net_segments.cursor
	is
		segment_cursor : pac_net_segments.cursor; -- to be returned

		procedure query_segments (strand : in type_strand) is
			segment_position : type_vector_model := far_upper_right;
			
			procedure query_segment (c : in pac_net_segments.cursor) is begin
				if get_A (c) < segment_position then
					segment_position := get_A (c);
					segment_cursor := c;
				end if;

				if get_B (c) < segment_position then
					segment_position := get_B (c);
					segment_cursor := c;
				end if;
			end query_segment;
			
		begin
			iterate (strand.segments, query_segment'access);
		end query_segments;
		
	begin
		query_element (
			position	=> strand_cursor,
			process		=> query_segments'access);
		
		return segment_cursor;
	end get_first_segment;



	

	function get_sheet (
		strand_cursor	: in pac_strands.cursor)
		return type_sheet
	is begin
		return get_sheet (element (strand_cursor).position);
	end get_sheet;
		

	
	

	function on_segment (
		segment_cursor	: in pac_net_segments.cursor;
		point			: in type_vector_model)
		return boolean
	is begin
		return element (segment_cursor).on_line (point);
	end on_segment;
	



	

	function on_strand (
		strand_cursor	: in pac_strands.cursor;
		place			: in type_object_position)
		return boolean
	is
		-- This flag goes false if the given point is
		-- on the given strand:
		proceed : aliased boolean := true;

		procedure query_segment (s : in pac_net_segments.cursor) is begin
			--if on_segment (s, type_vector_model (place)) then
			if on_segment (s, place.place) then
				proceed := false; -- abort iteration
			end if;
		end query_segment;
		
	begin
		-- The sheet number of strand and point must match:
		if get_sheet (strand_cursor) = get_sheet (place) then

			-- Probe the segments of the strand:
			iterate (
				segments	=> element (strand_cursor).segments, 
				process		=> query_segment'access,
				proceed		=> proceed'access);

		end if;

		-- Return false if sheet numbers do not match 
		-- or if point is not on any segment of the given strand:
		return (not proceed);
	end on_strand;


	
	

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



	

	procedure merge_strand (
		net		: in out type_net;
		strand	: in type_strand)
	is begin
		append (net.strands, strand);
	end merge_strand;



	

	procedure merge_strands (
		net		: in out type_net;
		strands	: in out pac_strands.list)
	is begin
		splice (
			target	=> net.strands,
			before	=> pac_strands.no_element,
			source	=> strands);
	end merge_strands;



	

	procedure merge_nets (
		net_1	: in out type_net;
		net_2	: in out type_net)
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
			target => net_1.strands, 
			before => pac_strands.no_element,
			source => net_2.strands);

		-- BOARD:
		
		-- conductor lines:
		splice (
			target => net_1.route.lines, 
			before => pac_conductor_lines.no_element,
			source => net_2.route.lines);

		-- conductor arcs:
		splice (
			target => net_1.route.arcs, 
			before => pac_conductor_arcs.no_element,
			source => net_2.route.arcs);

		-- vias:
		splice (
			target => net_1.route.vias, 
			before => pac_vias.no_element,
			source => net_2.route.vias);

		-- fill zones:
		splice (
			target => net_1.route.zones.solid, 
			before => pac_route_solid.no_element,
			source => net_2.route.zones.solid);
		
		splice (
			target => net_1.route.zones.hatched, 
			before => pac_route_hatched.no_element,
			source => net_2.route.zones.hatched);

		-- cutout areas:
		-- CS now net specific restrict stuff
		--splice (
			--target => net_1.route.cutouts, 
			--before => pac_cutouts.no_element,
			--source => net_2.route.cutouts);

		
	end merge_nets;


	


	function get_net_name (
		net_cursor : in pac_nets.cursor)
		return pac_net_name.bounded_string
	is begin
		return key (net_cursor);
	end;


	

	
	function to_string (
		net_cursor : in pac_nets.cursor)
		return string
	is begin
		return to_string (key (net_cursor));
	end to_string;


	


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
			iterate (element (segment_cursor).ports.devices, query_devices'access);

			-- Ports of netchangers and submodules go into the result right away
			-- because they are not affected by any assembly variants.
			union (result.netchangers, element (segment_cursor).ports.netchangers);
			union (result.submodules, element (segment_cursor).ports.submodules);
		end query_segments;

		
		procedure query_strands (strand_cursor : in pac_strands.cursor) is begin
			iterate (element (strand_cursor).segments, query_segments'access);
		end query_strands;

		
	begin
		--put_line ("net " & to_string (key (net)));		
		iterate (element (net).strands, query_strands'access);
		return result;
	end get_ports;


	

	

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
	





	
	function to_label_rotation (direction : in type_stub_direction) 
		return type_rotation_model is
	begin
		case direction is
			when RIGHT	=> return zero_rotation;
			when LEFT	=> return 180.0;
			when UP		=> return 90.0;
			when DOWN	=> return -90.0;
		end case;
	end to_label_rotation;



	
	
	function stub_direction (
		segment	: in pac_net_segments.cursor;
		point	: in type_vector_model)
		return type_stub 
	is
		is_stub : boolean := true;
		direction : type_stub_direction;
		orientation : constant type_net_segment_orientation := 
			get_segment_orientation (segment);

		-- Get the start and end point of the segment:
		A : constant type_vector_model := get_A (segment);
		B : constant type_vector_model := get_B (segment);
	begin
		case orientation is
			when HORIZONTAL =>
				if get_x (point) >= get_x (A) and
					get_x (point) >= get_x (B) then
					direction := RIGHT;
				end if;

				if get_x (point) <= get_x (A) and
					get_x (point) <= get_x (B) then
					direction := LEFT;
				end if;
				
			when VERTICAL =>
				if get_y (point) >= get_y (A) and
					get_y (point) >= get_y (B) then
					direction := UP;
				end if;

				if get_y (point) <= get_y (A) and
					get_y (point) <= get_y (B) then
					direction := DOWN;
				end if;
				
			when SLOPING =>
				is_stub := false;
		end case;

		if is_stub then
			return (is_stub => TRUE, direction => direction);
		else
			return (is_stub => FALSE);
		end if;

	end stub_direction;


	
end et_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
