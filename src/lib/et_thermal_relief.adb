------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           THERMAL RELIEF                                 --
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
--   to do:


with ada.text_io;				use ada.text_io;


package body et_thermal_relief is
	
	function to_string (connection : in type_pad_connection) return string is begin
		return to_lower (type_pad_connection'image (connection));
	end;

	
	function to_pad_connection (connection : in string) return type_pad_connection is begin
		return type_pad_connection'value (connection);
	end;

	
	
	function to_string (technology : in type_pad_technology) return string is begin
		return to_lower (type_pad_technology'image (technology));
	end;

	
	function to_pad_technology (technology : in string) return type_pad_technology is begin
		return type_pad_technology'value (technology);
	end to_pad_technology;
	


	

	function to_string (
		terminal : in type_terminal_with_relief)
		return string
	is begin
		return "absolute position: " & to_string (terminal.position)
			& " name: " & get_terminal_name (terminal.terminal);
			-- CS: outline ?
	end;


	

	
	function get_terminal_name (
		terminal : in pac_terminals_with_relief.cursor)
		return pac_terminal_name.bounded_string
	is 
		t : type_terminal_with_relief renames element (terminal);
	begin
		return get_terminal_name (t.terminal);
	end;



	function get_terminal_name (
		terminal : in pac_terminals_with_relief.cursor)
		return string
	is 
		t : type_terminal_with_relief renames element (terminal);
	begin
		return get_terminal_name (t.terminal);
	end;



	function to_string (
		terminal : in pac_terminals_with_relief.cursor)
		return string
	is 
		t : type_terminal_with_relief renames element (terminal);
	begin
		return to_string (t);
	end;

	

	

	procedure append_relieves (
		target	: in out pac_terminals_with_relief.list;
		source	: in pac_terminals_with_relief.list)
	is 
		scratch : pac_terminals_with_relief.list := source;
	begin
		target.splice (before => pac_terminals_with_relief.no_element,
					   source => scratch);
	end;




	
	

	function make_relief (
		zone				: in type_zone'class;
		relief_properties	: in type_relief_properties;					 
		terminal			: in type_terminal_with_relief;
		zone_clearance		: in type_track_clearance;
		zone_linewidth		: in type_track_width;
		log_threshold		: in type_log_level)
		return type_relief
	is
		debug : boolean := false;
		
		use pac_terminals;

		-- The center of the given terminal;
		center : type_vector renames terminal.position.place;

		-- The outline of the given terminal:
		outline : type_polygon renames terminal.outline;

		-- The direction of a spoke starting at the center of 
		-- the terminal:
		direction : type_angle := terminal.position.rotation;

		-- The thermal relief of the terminal to 
		-- be built:
		relief : type_relief;

		


-- 		procedure make_spoke is
-- 			-- For better understanding of this procedure:
-- 			-- Imagine a line that starts at the center of the terminal.
-- 			-- The line runs into the current direction and
-- 			-- 1. intersects the edge of the terminal
-- 			-- 2. travels through a non-conducting space (we call it "gap")
-- 			-- 3. intersects the outer edge of the surrounding conducting area
-- 			--    (This area is the island into which the terminal is embedded.)
-- 			-- 4. intersects the centerline of the border of the conducting area.
-- 			
-- 			-- Get the distance from the center of the terminal to the 
-- 			-- conducting area in the current direction:
-- 			D2CA : constant type_distance_to_conducting_area := 
-- 				get_distance_to_conducting_area (
-- 					zone			=> zone, 
-- 					linewidth		=> zone_linewidth,								
-- 					start_point		=> center, 
-- 					direction		=> direction,
-- 					location_known	=> true,
-- 					location		=> NON_CONDUCTING_AREA,
-- 					log_threshold	=> log_threshold + 3);		
-- 
-- 			-- The distance from the center to the edge
-- 			-- of the terminal:
-- 			center_to_terminal_edge : type_float_positive;
-- 
-- 			-- The distance from the center to the edge
-- 			-- of the surrounding conducting area:
-- 			center_to_conducting_area : type_float_positive;
-- 			
-- 			-- The gap between terminal edge and conducting area:
-- 			gap : type_distance_positive;
-- 			
-- 		begin
-- 			-- NOTE: There is no need to test whether the center 
-- 			-- of the terminal is in the non-conducting area of the zone.
-- 
-- 			-- If no centerline exists in the current direction,
-- 			-- then no spoke will be computed:
-- 			if D2CA.centerline_exists then
-- 				log (text => "D2CA: "
-- 					& "to edge: " & to_string (D2CA.distance_to_edge)
-- 					& " to centerline: " & to_string (D2CA.distance_to_centerline),
-- 					level => log_threshold + 2);
-- 
-- 				-- Compute the distance from the center of the terminal
-- 				-- to the edge of the terminal:
-- 				center_to_terminal_edge := 
-- 					get_distance_to_border (outline, center, direction);
-- 
-- 				-- Get the distance from the center of the terminal
-- 				-- to the edge of the conducting area:
-- 				center_to_conducting_area := D2CA.distance_to_edge;
-- 
-- 				-- Compute the actual gap between terminal edge and
-- 				-- conducting area:
-- 				gap := type_distance_positive (center_to_conducting_area - center_to_terminal_edge);
-- 				
-- 				-- CS: Due to the fill tolerance of zones, the gap might be
-- 				-- slightly too wide. For this reason the global fill_tolerance is subtracted.
-- 				-- The smaller the fill tolerance, the smaller is the error that develops here:
-- 				-- CS NOT SURE WHETHER THIS A WISE IDEA !!
-- 				gap := gap - fill_tolerance;
-- 				
-- 				log (text => "detected gap between terminal edge and conducting area: " 
-- 					 & to_string (gap), level => log_threshold + 2);
-- 		
-- 				-- If the gap is smaller or equal the given relief properties
-- 				-- then add a spoke that runs into the given direction:
-- 				if gap <= relief_properties.gap_max then
-- 					log (text => "add spoke", level => log_threshold + 2);
-- 
-- 					-- The spoke starts at the center of the terminal and
-- 					-- ends on the centerline of the border of the conducting area:
-- 					relief.spokes.append ((
-- 						A		=> center,
-- 						B		=> move_by (center, direction, D2CA.distance_to_centerline),
-- 						status	=> <>)); -- default status							
-- 				end if;
-- 			end if;
-- 		end make_spoke;


		
		
		procedure make_spoke_2 is
			-- For better understanding of this procedure:
			-- Imagine a line that starts at the center of the terminal.
			-- The line runs into the current direction and
			-- 1. intersects the edge of the terminal
			-- 2. travels through a non-conducting space (we call it "gap")
			-- 3. intersects the outer edge of the surrounding conducting area
			--    (This area is the island into which the terminal is embedded.)
			-- 4. intersects the centerline of the border of the conducting area.
			
			-- The distance from the center to the edge
			-- of the terminal:
			center_to_terminal_edge : type_float_positive;

			-- The distance from the center to the edge
			-- of the surrounding conducting area:
			center_to_conducting_area : type_float_positive;
			
			border_exists : boolean;
			
			-- The gap between terminal edge and conducting area:
			gap : type_distance_positive;
			
		begin
			log (text => "make_spoke", level => log_threshold + 2);
			log_indentation_up;
			
			-- NOTE: There is no need to test whether the center 
			-- of the terminal is in the non-conducting area of the zone.

			-- Get the distance from the center of the terminal to the 
			-- conducting area in the current direction:
			get_distance_to_border (
				zone			=> zone,
				point			=> center,
				direction		=> direction,
				border_exists	=> border_exists,
				distance		=> center_to_conducting_area,
				log_threshold	=> log_threshold + 3);
			
			-- If a border exists in the current direction,
			-- then a spoke will be computed:
			if border_exists then
				log (text => "distance to border: " & to_string (center_to_conducting_area),
					level => log_threshold + 2);

				-- Compute the distance from the center of the terminal
				-- to the edge of the terminal:
				center_to_terminal_edge := 
					get_distance_to_border (outline, center, direction);

				-- Compute the actual gap between terminal edge and
				-- conducting area:
				gap := type_distance_positive (center_to_conducting_area - center_to_terminal_edge);
				gap := gap - zone_linewidth * 0.5;
				
				-- CS: Due to the fill tolerance of zones, the gap might be
				-- slightly too wide. For this reason the global fill_tolerance is subtracted.
				-- The smaller the fill tolerance, the smaller is the error that develops here:
				-- CS NOT SURE WHETHER THIS A WISE IDEA !!
				gap := gap - fill_tolerance;
				
				log (text => "detected gap between terminal edge and conducting area: " 
					 & to_string (gap), level => log_threshold + 2);
		
				-- If the gap is smaller or equal the given relief properties
				-- then add a spoke that runs into the given direction:
				if gap <= relief_properties.gap_max then
					log (text => "add spoke", level => log_threshold + 2);

					-- The spoke starts at the center of the terminal and
					-- ends on the centerline of the border of the conducting area:
					relief.spokes.append ((
						A		=> center,
						B		=> move_by (center, direction, center_to_conducting_area),
						status	=> <>)); -- default status							
				end if;
			end if;
			
			log_indentation_down;
		end make_spoke_2;

		
		
	begin
		log (text => "make_relief ",
			level => log_threshold);
		
		log_indentation_up;
		
		-- CS: 
		-- 1. test thermal_relief on/off flag (see et_terminals.type_terminal)
		--    If off, then no thermal relief is to be generated. Skip terminal completely.
		-- 2. Limit maximum spoke width to drill size, pad size, ... ?
		-- 3. If a spoke width greater than the zone_linewidth is required then the
		--    current simple concept of type_relief must be extended. A spoke would then
		--    consist of several parallel lines of zone_linewidth.
		
		relief.width := zone_linewidth;
		
		-- Ensure a minimum width of the spokes:
		limit_to_minimum (relief.width, relief_properties.width_min);
		

		-- if debug then
		-- 	new_line;
		-- 	put_line ("terminal " & to_string (key (terminal.terminal))
		-- 		& " pos. " & to_string (terminal.position.place)
		-- 		& " direction " & to_string (terminal.position.rotation)
		-- 		& " gap max. " & to_string (relief_properties.gap_max)
		-- 		& " linewidth " & to_string (relief.width));
		-- end if;

		-- Since we have to generate 4 spokes, separated by 90 degrees
		-- from each other, we need a loop here:
		for i in 1 .. 4 loop
			
			log (text => "spoke: " & positive'image (i)
				& " direction: " & to_string (direction), level => log_threshold + 1);

			log_indentation_up;
			make_spoke_2;
			log_indentation_down;

			-- Prepare the next spoke:
			add (direction, 90.0);
		end loop;

		
		log_indentation_down;
		
		return relief;
	end make_relief;



	
	

	
	function make_reliefes (
		zone				: in type_zone'class;
		relief_properties	: in type_relief_properties;
		terminals			: in pac_terminals_with_relief.list;
		zone_clearance		: in type_track_clearance;
		zone_linewidth		: in type_track_width;
		log_threshold		: in type_log_level)
		return pac_reliefes.list
	is
		result : pac_reliefes.list;

		debug : boolean := false;
		-- debug : boolean := true;
		
		use pac_terminals_with_relief;

		
		procedure query_terminal (
			c : in pac_terminals_with_relief.cursor) 
		is 
			terminal : type_terminal_with_relief renames element (c);
			relief : type_relief;			
		begin
			log (text => "terminal: " & to_string (terminal),
				 level => log_threshold + 1);

			log_indentation_up;
			
			-- Generate the thermal relief for the
			-- candidate terminal:
			relief := make_relief (
				zone				=> zone,
				relief_properties	=> relief_properties,
				terminal			=> terminal,
				zone_clearance		=> zone_clearance, 
				zone_linewidth		=> zone_linewidth,
				log_threshold		=> log_threshold + 1);

			-- Append the relief to the result:
			result.append (relief);

			log_indentation_down;
		end query_terminal;

		
	begin
		log (text => "make_reliefes."
			 & " zone linewidth: " & to_string (zone_linewidth)
			 & " zone clearance: " & to_string (zone_clearance),
			 -- CS & " properties: " & to_string (relief_properties),
			 level => log_threshold);
		
		log_indentation_up;
		
		terminals.iterate (query_terminal'access);

		log_indentation_down;
		
		return result;
	end make_reliefes;

	
	
end et_thermal_relief;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
