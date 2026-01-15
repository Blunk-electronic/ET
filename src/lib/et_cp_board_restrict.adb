------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--             COMMAND PROCESSOR / BOARD / ROUTE AND VIA RESTRICT           --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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
-- To Do:
-- - rework
--
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_directions;						use et_directions;
with et_primitive_objects;				use et_primitive_objects;

with et_keywords;						use et_keywords;

with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_board_geometry;					use et_board_geometry;
with et_board_ops.route_restrict;
with et_board_ops.via_restrict;
with et_keywords;



package body et_cp_board_restrict is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_contours;

	

	
	procedure draw_route_restrict (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_board_ops.route_restrict;
		
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);



		-- Extract from the given command the zone arguments (everything after "zone"):
		-- example command: board demo draw route_restrict [1] zone line 0 0 line 50 0 line 50 50 line 0 50
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			l : pac_signal_layers.set;
		begin
			l := to_layers (get_field (cmd, 5));
			
			draw_zone (
				module_cursor	=> module,
				zone			=> (c with l),
				log_threshold	=> log_threshold + 1);

		end build_zone;
		
		
		shape : type_shape;

		line_tmp	: type_line;
		arc_tmp		: type_arc;
		circle_tmp	: type_circle;
		layers_tmp	: pac_signal_layers.set;
		
		
	begin
		-- CS log message


		if get_field (cmd, 6) = keyword_zone then
			build_zone;
		else
			shape := to_shape (get_field (cmd, 6));
			-- CS apply assigment to shape to all similar procedures !			

			
			case shape is
				when LINE =>
					case cmd_field_count is
						when 10 =>
							-- board led_driver draw route_restrict [1,3,5-9] line 10 10 60 10
							-- CS board led_driver draw route_restrict 3 line 10 10 60 10
							
							-- CS test whether field 5 is a single layer id. If yes then
							-- call function et_pcb_stack.to_signal_layer to get the id-type.
							-- Then validate signal layer.
							-- Then add the single signal layer to a set.
							-- Do so with all objects in route and via restrict.

							line_tmp := type_line (to_line (
								A => to_vector_model (get_field (cmd, 7), get_field (cmd, 8)),
								B => to_vector_model (get_field (cmd, 9), get_field (cmd, 10))));

							layers_tmp := to_layers (get_field (cmd, 5)); -- [1,3,5-9]
							
							draw_route_restrict_line (
								module_name 	=> key (module),
								line			=> (line_tmp with layers_tmp),
								log_threshold	=> log_threshold + 1);

						when 11 .. type_field_count'last => 
							command_too_long (cmd, cmd_field_count - 1);
							
						when others => command_incomplete (cmd);
					end case;

					
				when ARC =>
					case cmd_field_count is
						when 13 =>							
							-- board led_driver draw route_restrict [1,3,5-9] arc 50 50 0 50 100 0 cw

							layers_tmp := to_layers (get_field (cmd, 5)); -- [1,3,5-9]
							
							arc_tmp := type_arc (to_arc (
								center		=> to_vector_model (get_field (cmd, 7), get_field (cmd, 8)),
								A			=> to_vector_model (get_field (cmd, 9), get_field (cmd, 10)),
								B			=> to_vector_model (get_field (cmd, 11), get_field (cmd, 12)),
								direction	=> to_direction (get_field (cmd, 13))));
															
							draw_route_restrict_arc (
								module_name 	=> key (module),
								arc				=> (arc_tmp with layers_tmp),
								log_threshold	=> log_threshold + 1);

						when 14 .. type_field_count'last => 
							command_too_long (cmd, cmd_field_count - 1);
							
						when others => command_incomplete (cmd);
					end case;

					
				when CIRCLE =>
					case cmd_field_count is
						when 9 =>
							-- board led_driver draw route_restrict [1,3,5-9] circle 20 50 40
							-- if is_number (get_field (cmd, 7)) then -- 20

							layers_tmp := to_layers (get_field (cmd, 5)); -- [1,3,5-9]

							circle_tmp := type_circle (to_circle (
								center	=> to_vector_model (get_field (cmd, 7), get_field (cmd, 8)),
								radius	=> to_radius (get_field (cmd, 9)))); -- 40
																	
								-- Circle is not filled.
								draw_route_restrict_circle (
									module_name 	=> key (module),
									circle			=> (circle_tmp with layers_tmp),
									log_threshold	=> log_threshold + 1);
								
							-- else
								-- expect_value_center_x (7);
							-- end if;

						--when 10 =>
							---- Circle is filled.
							---- board led_driver draw route_restrict [1,3,5-9] circle filled 20 50 40
							--if get_field (cmd, 7) = keyword_filled then

								---- Circle is filled.
								--draw_route_restrict_circle (
									--module_name 	=> module,
									--circle			=> 
												--(
												--layers		=> to_layers (get_field (cmd, 5)), -- [1,3,5-9]
												--filled		=> YES,
												--center	=> to_vector_model (get_field (cmd, 8), get_field (cmd, 9))),
												--radius	=> to_radius (get_field (cmd, 10)) -- 40
												--),
												
									--log_threshold	=> log_threshold + 1);
							--else
								--expect_keyword_filled (7);
							--end if;

						when 10 .. type_field_count'last => 
							command_too_long (cmd, cmd_field_count - 1);
						
						when others => command_incomplete (cmd);
					end case;
							
				when others => null;
			end case;
		end if;

	end draw_route_restrict;





	



	procedure draw_via_restrict (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_board_ops.via_restrict;

		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);

		
		-- Extract from the given command the zone arguments (everything after "zone"):
		-- example command: board demo draw via_restrict [1] zone line 0 0 line 50 0 line 50 50 line 0 50
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			l : pac_signal_layers.set;
		begin
			l := to_layers (get_field (cmd, 5));
			
			draw_zone (
				module_cursor	=> module,
				zone			=> (c with l),
				log_threshold	=> log_threshold + 1);

		end build_zone;

			
	begin
		-- CS log message
		
		if get_field (cmd, 6) = keyword_zone then
			build_zone;
		else
			null;
			-- CS error. only zone allowed here
		end if;		
	end draw_via_restrict;





	


	procedure delete_route_restrict (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_board_ops.route_restrict;

		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure do_it is
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (cmd, 5), get_field (cmd, 6)),
				radius	=> to_zone_radius (get_field (cmd, 7)));
											
			delete_route_restrict (
				module_name 	=> key (module),
				catch_zone		=> catch_zone,
				log_threshold	=> log_threshold + 1);

		end do_it;

		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 7 => do_it;
			
			when 8 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end delete_route_restrict;
	







	procedure delete_via_restrict (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_board_ops.via_restrict;

		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		procedure do_it is
			catch_zone : type_catch_zone;
		begin
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (cmd, 5), get_field (cmd, 6)),
				radius	=> to_zone_radius (get_field (cmd, 7)));

			-- CS
			-- delete_via_restrict (
			-- 	module_name 	=> key (module),
			-- 	catch_zone		=> catch_zone,
			-- 	log_threshold	=> log_threshold + 1);

		end do_it;
		
	begin
		-- CS log message
		
		case cmd_field_count is
			when 7 => do_it;
			
			when 8 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end delete_via_restrict;

	
	
end et_cp_board_restrict;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
