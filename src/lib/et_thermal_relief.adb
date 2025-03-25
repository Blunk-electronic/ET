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
	

														 

	function make_relief (
		zone				: in type_zone'class;
		relief_properties	: in type_relief_properties;					 
		terminal_cursor		: in pac_terminals_with_relief.cursor;
		zone_clearance		: in type_track_clearance;
		zone_linewidth		: in type_track_width;
		debug				: in boolean := false)
		return type_relief
	is
		use pac_terminals;
		use pac_terminals_with_relief;
		terminal : type_terminal_with_relief renames element (terminal_cursor);
		
		center : type_vector renames terminal.position.place;
		outline : type_polygon renames terminal.outline;
		
		angle : type_angle := terminal.position.rotation;
		relief : type_relief;
			
	begin
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
		

		if debug then
			new_line;
			put_line ("terminal " & to_string (key (terminal.terminal))
				& " pos. " & to_string (terminal.position.place)
				& " angle " & to_string (terminal.position.rotation)
				& " gap max. " & to_string (relief_properties.gap_max)
				& " linewidth " & to_string (relief.width));
		end if;

		-- Since we have to generate 4 spokes, separated by 90 degrees
		-- from each other, we need a loop construct here:
		for i in 1 .. 4 loop
			if debug then
				put_line (" spoke " & positive'image (i));
				put_line ("  direction " & to_string (angle));
			end if;
				
			declare
				-- Get the distance from center of terminal to the 
				-- conducting area in the current direction:
				D2CA : constant type_distance_to_conducting_area := 
					get_distance_to_conducting_area (
						zone			=> zone, 
						start_point		=> center, 
						direction		=> angle,
						location_known	=> true,
						location		=> NON_CONDUCTING_AREA,
						debug			=> false);
						--debug			=> true);						

				-- The distance from edge to centerline:
				border_to_centerline : type_float_positive;

				-- The actual gap between terminal and conducting area:
				gap : type_float_positive;
				
			begin
				-- NOTE: There is no need to test whether the center of the terminal
				-- is in the non-conducting area of the zone.

				-- If no centerline exists in the current direction,
				-- then no spoke will be computed:
				if D2CA.centerline_exists then

					if debug then
						put_line ("  distance to conducting area:");
						
						put_line ("   to edge       " 
							& to_string (D2CA.distance_to_edge));

						put_line ("   to centerline " 
							& to_string (D2CA.distance_to_centerline));
						
					end if;

					-- The spoke length is the distance from center of terminal
					-- to centerline of border.
					-- If the resulting gap between terminal and conducting area
					-- is less than the gap_max of the relief_properties, then
					-- append the spoke to the relief:
					border_to_centerline := D2CA.distance_to_centerline - D2CA.distance_to_edge;

					gap := D2CA.distance_to_centerline - border_to_centerline
						   - get_distance_to_border (outline, center, angle);

					if debug then
						put_line ("  gap between pad and conducting area " & to_string (gap));						
					end if;

					
					if gap <= type_float_positive (relief_properties.gap_max) then
						if debug then
							put_line ("   generate relief spoke");
						end if;
						
						relief.spokes.append ((
							start_point	=> center,
							end_point	=> move_by (center, angle, D2CA.distance_to_centerline),
							status		=> <>)); -- default status							
					end if;
				end if;

				
				-- Prepare the next spoke:
				add (angle, 90.0);
			end;
		end loop;

		return relief;
	end make_relief;


	

	
	function make_reliefes (
		zone				: in type_zone'class;
		relief_properties	: in type_relief_properties;
		terminals			: in pac_terminals_with_relief.list;
		zone_clearance		: in type_track_clearance;
		zone_linewidth		: in type_track_width)
		return pac_reliefes.list
	is
		result : pac_reliefes.list;

		debug : boolean := false;
		-- debug : boolean := true;
		
		use pac_terminals_with_relief;

		procedure query_terminal (c : in pac_terminals_with_relief.cursor) is
		begin
			result.append (make_relief (
				zone, relief_properties, c, zone_clearance, zone_linewidth, debug));
		end query_terminal;
		
	begin
		terminals.iterate (query_terminal'access);
		return result;
	end make_reliefes;

	
	
end et_thermal_relief;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
