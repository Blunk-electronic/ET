------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / BOARD CONTOURS                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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


package body et_board_ops.board_contour is

	use pac_generic_modules;
	
	procedure set_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		outline			: in type_outer_contour;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.contours.outline := outline;
		end;
							   
	begin
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			 & " setting outline" & to_string (outline),
			level => log_threshold);

		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end set_outline;



	function get_outline (
		module_cursor	: in pac_generic_modules.cursor)
		return type_outer_contour
	is begin
		return element (module_cursor).board.contours.outline;
	end get_outline;


	
	function get_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level)
		return type_outer_contour
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being queried
	begin
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			 & " getting outline",
			level => log_threshold);

		module_cursor := locate_module (module_name);
		
		return get_outline (module_cursor);
	end get_outline;



	procedure delete_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_vector_model; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is			
			deleted : boolean := false; -- goes true if at least one segment has been deleted

			procedure delete_segment is 
				use pac_segments;
				c : pac_segments.cursor;
			begin
				c := module.board.contours.outline.contour.segments.first;
				
				while c /= pac_segments.no_element loop

					case element (c).shape is
						when LINE =>
							if element (c).segment_line.on_line (point) then
								-- CS use get_shortest_distance (point, element)
								-- and compare distance with accuracy	

								delete (module.board.contours.outline.contour.segments, c);
								deleted := true;

								-- CS update start/end point of predecessor/successor segment
								
								exit; -- CS no exit if all segments are to be deleted
							end if;

						when ARC =>
							if element (c).segment_arc.on_arc (point) then
								-- CS use get_shortest_distance (point, element)
								-- and compare distance with accuracy	

								delete (module.board.contours.outline.contour.segments, c);
								deleted := true;

								-- CS update start/end point of predecessor/successor segment
								
								exit; -- CS no exit if all segments are to be deleted
							end if;

					end case;
					
					next (c);
				end loop;
			end delete_segment;

			procedure delete_circle is begin
				if module.board.contours.outline.contour.circle.on_circle (point) then
					-- CS use get_shortest_distance (point, element)
					-- and compare distance with accuracy	

					module.board.contours.outline.contour := (others => <>);					
					deleted := true;
				end if;
			end delete_circle;

			
		begin -- delete
			if module.board.contours.outline.contour.circular then
				delete_circle;				
			else
				delete_segment;
			end if;
			
			if not deleted then
				nothing_found (point, accuracy);
			end if;			
		end delete;

		
	begin -- delete_outline
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			& " deleting outline segment at" & to_string (point) 
			& " accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_outline;

	

	procedure add_hole (
		module_cursor	: in pac_generic_modules.cursor;
		hole			: in type_hole;
		log_threshold	: in type_log_level)
	is

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is 
			use pac_holes;
		begin
			append (module.board.contours.holes, hole);
		end;
							   
	begin
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) 
			 & " adding hole" & to_string (hole),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end add_hole;

	
	
	procedure add_hole (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		hole			: in type_hole;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is 
			use pac_holes;
		begin
			append (module.board.contours.holes, hole);
		end;
							   
	begin
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			 & " adding hole" & to_string (hole),
			level => log_threshold);

		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end add_hole;


	function get_holes (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_holes.list
	is begin
		return element (module_cursor).board.contours.holes;
	end get_holes;
	


	procedure delete_hole (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_vector_model; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

	begin -- delete_hole
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			& " deleting hole segment at" & to_string (point) 
			& " accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		module_cursor := locate_module (module_name);

		--update_element (
			--container	=> generic_modules,
			--position	=> module_cursor,
			--process		=> delete'access);

		-- CS
		
	end delete_hole;



	
end et_board_ops.board_contour;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
