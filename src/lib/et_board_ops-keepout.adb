------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / KEEPOUT                             --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 



package body et_board_ops.keepout is


	procedure add_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_keepout_zone;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
		-- When searching among already existing zones then
		-- this flag is used to abort the iteration prematurely:
		proceed : boolean := true;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			use pac_keepout_zones;
			c : pac_keepout_zones.cursor;

			-- This procedure tests whether the candidate
			-- zone z is open. If z is open, then it tries
			-- to merge the given zone into z. If the merge operation
			-- succeedes then no more zones are iterated (flag proceed):
			procedure query_zone (z : in out type_keepout_zone) is
				use et_board_shapes_and_text;
				use pac_contours;
				mr : type_merge_result;
			begin
				-- put_line ("query_zone");
				if is_open (zone) then
					--put_line (" is open");
					merge_contours (z, zone, mr);
					if mr.successful then
						--put_line ("  successful");
						-- No more searching needed -> abort iterator
						proceed := false;
					end if;
				end if;
			end query_zone;

			
		begin
			case face is
				when TOP =>
					-- Iterate through the already existing zones:
					c := module.board.keepout.top.zones.first;

					while c /= pac_keepout_zones.no_element and proceed loop
						module.board.keepout.top.zones.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						-- put_line ("added as new zone");
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.keepout.top.zones.append (zone);
					end if;

					
				when BOTTOM =>
					-- Iterate through the already existing zones:
					c := module.board.keepout.bottom.zones.first;

					while c /= pac_keepout_zones.no_element and proceed loop
						module.board.keepout.bottom.zones.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.keepout.bottom.zones.append (zone);
					end if;

			end case;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor) 
			 & " drawing keepout zone" 
			 & to_string (face)
			 & " " & to_string (contour => zone, full => true),
			level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end add_zone;







	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_keepout_zones;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				modify_status (segment, operation);
			end query_segment;

			
			procedure query_zone (
				zone : in out type_keepout_zone)
			is begin
				if zone.contour.circular then
					null; -- CS
				else
					-- Locate the given segment in the
					-- candidate zone:
					update_element (
						container	=> zone.contour.segments,
						position	=> segment.segment,
						process		=> query_segment'access);

				end if;
			end query_zone;
	
			
		begin
			-- Search the given segment according to its
			-- zone and face:
			case segment.face is
				when TOP =>
					update_element (
						container	=> module.board.keepout.top.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

				when BOTTOM =>
					update_element (
						container	=> module.board.keepout.bottom.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

			end case;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (segment.segment)
			& " face " & to_string (segment.face)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;


	


	procedure propose_segments (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_vector_model;
		zone			: in type_accuracy;
		face			: in type_face;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_keepout_zones;
			zc : pac_keepout_zones.cursor;

			use pac_contours;
			use pac_segments;

			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				case segment.shape is
					when LINE =>
						if within_accuracy (
							line	=> segment.segment_line,
							width	=> zero,
							point	=> point,
							zone	=> zone)
						then
							set_proposed (segment);
							count := count + 1;
							log (text => to_string (segment), level => log_threshold + 1);
						end if;
   
					when ARC =>
						null; -- CS
				end case;
			end query_segment;


			
			procedure query_zone (
				zone : in out type_keepout_zone)
			is
				use pac_contours;
				use pac_segments;
				c : pac_segments.cursor;
				
			begin
				if zone.contour.circular then
					null; -- CS
				else
					c := zone.contour.segments.first;

					while c /= pac_segments.no_element loop
						update_element (
							container	=> zone.contour.segments,
							position	=> c,
							process		=> query_segment'access);

						next (c);
					end loop;
				end if;
			end query_zone;
			
			
		begin
			case face is
				when TOP =>
					zc := module.board.keepout.top.zones.first;

					while zc /= pac_keepout_zones.no_element loop
						update_element (
							container	=> module.board.keepout.top.zones,
							position	=> zc,
							process		=> query_zone'access);
						
						next (zc);
					end loop;

					
				when BOTTOM =>
					zc := module.board.keepout.bottom.zones.first;

					while zc /= pac_keepout_zones.no_element loop
						update_element (
							container	=> module.board.keepout.bottom.zones,
							position	=> zc,
							process		=> query_zone'access);
						
						next (zc);
					end loop;
			end case;	
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " proposing segments at " & to_string (point)
			 & " face " & to_string (face)
			 & " zone " & accuracy_to_string (zone),
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_segments;


	


	
	
	procedure reset_proposed_segments (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_keepout_zones;
			zc : pac_keepout_zones.cursor;

			use pac_contours;
			use pac_segments;

			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				reset_status (segment);
			end query_segment;


			
			procedure query_zone (
				zone : in out type_keepout_zone)
			is
				use pac_contours;
				use pac_segments;
				c : pac_segments.cursor;
				
			begin
				if zone.contour.circular then
					null; -- CS
				else
					c := zone.contour.segments.first;

					while c /= pac_segments.no_element loop
						update_element (
							container	=> zone.contour.segments,
							position	=> c,
							process		=> query_segment'access);

						next (c);
					end loop;
				end if;
			end query_zone;
			
			
		begin
			zc := module.board.keepout.top.zones.first;

			while zc /= pac_keepout_zones.no_element loop
				update_element (
					container	=> module.board.keepout.top.zones,
					position	=> zc,
					process		=> query_zone'access);
				
				next (zc);
			end loop;

					
			zc := module.board.keepout.bottom.zones.first;

			while zc /= pac_keepout_zones.no_element loop
				update_element (
					container	=> module.board.keepout.bottom.zones,
					position	=> zc,
					process		=> query_zone'access);
				
				next (zc);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " resetting proposed segments of zones in keepout",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_segments;




	function get_first_segment (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_segment
	is
		use pac_contours;
		result : type_object_segment;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_contours;
			use pac_segments;
			use pac_keepout_zones;
			
			proceed : aliased boolean := true;

			face : type_face := TOP;
			
			
			procedure query_zone (z : in pac_keepout_zones.cursor) is 

				procedure query_segment (
					c : in pac_segments.cursor) 
				is begin
					case flag is
						when PROPOSED =>
							if is_proposed (c) then
								result.segment := c;
								result.zone := z;
								result.face := face;
								proceed := false;

								log (text => to_string (c), level => log_threshold + 1);
							end if;

						when SELECTED =>
							if is_selected (c) then
								result.segment := c;
								result.zone := z;
								result.face := face;
								proceed := false;

								log (text => to_string (c), level => log_threshold + 1);
							end if;

						when others =>
							null; -- CS
					end case;
				end query_segment;
				
				
				procedure query_segments (z : in type_keepout_zone) is begin
					iterate (
						segments	=> z.contour.segments,
						process		=> query_segment'access,
						proceed		=> proceed'access);				
				end query_segments;

				
			begin
				if element (z).contour.circular then
					null; -- CS
				else
					query_element (z, query_segments'access);
				end if;
			end query_zone;

			
		begin
			-- Iterate the zones in top layer:
			iterate (
				zones	=> module.board.keepout.top.zones,
				process	=> query_zone'access, 
				proceed	=> proceed'access);

			
			-- If nothing found, iterate the bottom layer:
			if proceed then
				face := BOTTOM;
				
				iterate (
					zones	=> module.board.keepout.bottom.zones,
					process	=> query_zone'access, 
					proceed	=> proceed'access);

			end if;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first segment / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- put_line ("found " & to_string (result));
		
		log_indentation_down;

		return result;
	end get_first_segment;






	procedure move_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_keepout_zones;
				
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			-- Moves the candidate segment:
			procedure do_it (s : in out type_segment) is begin
				case s.shape is
					when LINE =>
						move_line_to (s.segment_line, point_of_attack, destination);

					when ARC =>
						null;
						-- CS
				end case;
			end do_it;

			
			procedure query_zone (
				zone : in out type_keepout_zone)
			is 
				c : pac_segments.cursor;
			begin
				if zone.contour.circular then
					null; -- CS
				else
					-- Locate the given segment in 
					-- the candidate zone:
					update_element (
						container	=> zone.contour.segments,
						position	=> segment.segment,
						process		=> do_it'access);

				end if;
			end query_zone;
	
			
		begin
			-- Search for the given segment according to the 
			-- given zone and face:
			case segment.face is
				when TOP =>
					update_element (
						container	=> module.board.keepout.top.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

				when BOTTOM =>
					update_element (
						container	=> module.board.keepout.bottom.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

			end case;
		end query_module;
		
				
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving keepout zone segment " & to_string (segment.segment)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);

		-- log (text => "new outline:" & to_string (get_outline (module_cursor), true),
		-- 	 level => log_threshold + 1);
		
		log_indentation_down;
	end move_segment;


	



	procedure delete_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_keepout_zones;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_zone (
				zone : in out type_keepout_zone)
			is 
				c : pac_segments.cursor;
			begin
				if zone.contour.circular then
					null; -- CS
				else
					-- Delete the given segment:
					c := segment.segment;					
					zone.contour.segments.delete (c);
				end if;
			end query_zone;
	
			
		begin
			-- Search for the given segment according to the 
			-- given zone and face:
			case segment.face is
				when TOP =>
					update_element (
						container	=> module.board.keepout.top.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

				when BOTTOM =>
					update_element (
						container	=> module.board.keepout.bottom.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting keepout zone segment " 
			& to_string (segment.segment),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end delete_segment;
	






	

	function get_count (
		objects : in pac_objects.list)
		return natural
	is begin
		return natural (objects.length);
	end get_count;
	
	
	
	

	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object
	is
		result_category 	: type_object_category := CAT_VOID;
		result_segment  	: type_object_segment;

		use pac_contours;
		use pac_segments;

	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;


		-- SEARCH FOR A SEGMENT OF A ZONE:
		
		-- If there is one, then go to the end  of this procedure:
		result_segment := get_first_segment (module_cursor, flag, log_threshold + 1);

		if result_segment.segment /= pac_segments.no_element then
			-- A segment has been found.
			log (text => to_string (result_segment.segment)
					& " face " & to_string (result_segment.face),
					level => log_threshold + 1);
			
			result_category := CAT_ZONE_SEGMENT;
		end if;

		
		-- If still nothing has been found then the category is CAT_VOID.
		

	<<end_of_search>>
		
		log_indentation_down;

		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_ZONE_SEGMENT =>
				return (CAT_ZONE_SEGMENT, result_segment);
				
		end case;
	end get_first_object;



	
	
	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return pac_objects.list
	is
		use pac_objects;
		result : pac_objects.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_keepout_zones;
			zone_cursor : pac_keepout_zones.cursor;
			face : type_face := TOP;
			
			
			procedure query_zone (zone : in type_keepout_zone) is
				use pac_contours;
				use pac_segments;
				-- CS test circular flag !!
				segment_cursor : pac_segments.cursor := zone.contour.segments.first;
				
				procedure query_segment (segment : in type_segment) is 

					procedure collect is begin
						result.append ((
							cat		=> CAT_ZONE_SEGMENT,
							segment	=> (face, zone_cursor, segment_cursor)));

						log (text => to_string (segment), level => log_threshold + 2);
					end collect;

				begin
					case flag is
						when PROPOSED =>
							if is_proposed (segment) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (segment) then
								collect;
							end if;
							
						when others => null; -- CS
					end case;
				end query_segment;
				
			begin
				while segment_cursor /= pac_segments.no_element loop
					query_element (segment_cursor, query_segment'access);
					next (segment_cursor);
				end loop;
			end query_zone;
			
			
		begin
			log (text => "top zones", level => log_threshold + 1);
			log_indentation_up;
			
			zone_cursor := module.board.keepout.top.zones.first;
			while zone_cursor /= pac_keepout_zones.no_element loop
				query_element (zone_cursor, query_zone'access);
				next (zone_cursor);
			end loop;

			log_indentation_down;

			
			face := BOTTOM;

			log (text => "bottom zones", level => log_threshold + 1);
			log_indentation_up;
			
			zone_cursor := module.board.keepout.bottom.zones.first;
			while zone_cursor /= pac_keepout_zones.no_element loop
				query_element (zone_cursor, query_zone'access);
				next (zone_cursor);
			end loop;

			log_indentation_down;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up objects / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element ( -- CS query_module is sufficient
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;

		return result;
	end get_objects;
	




	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of object"
			-- & to_string (segment.segment) CS output object category ?
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		case object.cat is
			when CAT_ZONE_SEGMENT =>
				modify_status (module_cursor, object.segment, operation, log_threshold + 1);
			
			when CAT_VOID =>
				null; -- CS
		end case;

		log_indentation_down;
	end modify_status;

	

	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is 
		use pac_objects;
		object : constant type_object := element (object_cursor);
	begin
		modify_status (module_cursor, object, operation, log_threshold);
	end modify_status;



	

	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " moving keepout object " 
			-- CS & to_string (object)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_ZONE_SEGMENT =>
				move_segment (module_cursor,
					object.segment,
					point_of_attack, destination,
					log_threshold + 1);
							
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end move_object;
	


	

	procedure reset_proposed_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor) &
			" resetting proposed objects",
			level => log_threshold);

		log_indentation_up;
		reset_proposed_segments (module_cursor, log_threshold + 1);
		log_indentation_down;
	end reset_proposed_objects;


	
	


	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " deleting keepout object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_ZONE_SEGMENT =>
				delete_segment (
					module_cursor	=> module_cursor, 
					segment			=> object.segment,
					log_threshold	=> log_threshold + 1);

				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end delete_object;
	


	
	
	
	
end et_board_ops.keepout;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
