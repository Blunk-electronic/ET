------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / NETCHANGERS                       --
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
-- To Do: 
--
--


with ada.text_io;						use ada.text_io;
with ada.containers;

with ada.exceptions;					use ada.exceptions;
with et_string_processing;				use et_string_processing;

with et_module;							use et_module;
with et_schematic_ops_netchangers;		use et_schematic_ops_netchangers;

with et_netchangers.board;				use et_netchangers.board;


package body et_board_ops_netchangers is


	use pac_netchangers;
	

	
	

	procedure move_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id;
		coordinates		: in type_coordinates;
		point			: in type_vector_model;
		log_threshold	: in type_log_level) 
	is

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			netchanger_cursor : pac_netchangers.cursor;
			
			
			procedure move (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) 
			is 
				place : type_vector_model;
			begin
				-- calculate the new position 
				case coordinates is
					when ABSOLUTE =>
						-- The absolute position is defined 
						-- by the given point (x/y):
						set_place (netchanger, point);

					when RELATIVE =>
						-- The relative position is the 
						-- netchanger position BEFORE 
						-- the move operation shifted by 
						-- the given point (x/y):
						place := get_place (netchanger);
						add (place, point);
						set_place (netchanger, place);
				end case;
			end move;

			
		begin
			-- Locate given netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);

			-- Move the netchanger:
			update_element (
				container	=> module.netchangers,
				position	=> netchanger_cursor,
				process		=> move'access);

		end query_module;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					& " move netchanger " & to_string (index) 
					& " to " & to_string (point),
					 level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
					& " move netchanger " & to_string (index) 
					& " by " & to_string (point),
					level => log_threshold);
		end case;

		
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end move_netchanger;




	



	


	procedure set_netchanger_layer (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		layer			: in type_signal_layer; -- 8
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			netchanger_cursor : pac_netchangers.cursor;
			
			
			procedure set_layer (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) 
			is begin
				set_layer (netchanger, layer);
			end set_layer;

			
		begin
			-- Locate given netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);

			update_element (
				container	=> module.netchangers,
				position	=> netchanger_cursor,
				process		=> set_layer'access);

		end query_module;


		
	begin
		log (text => "module " & to_string (module_cursor)
			& " set netchanger " & to_string (index) 
			& " signal layer to " & to_string (layer),
			level => log_threshold);


		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;		
	end set_netchanger_layer;


	


	
	

	function get_object_name (
		object : in type_object_netchanger)
		return string
	is begin
		return get_netchanger_name (object.netchanger_cursor);
	end;

	
	
	function get_object_id (
		object : in type_object_netchanger)
		return type_netchanger_id
	is begin
		return get_netchanger_id (object.netchanger_cursor);
	end;


	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		netchanger		: in type_object_netchanger;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is	
			
			procedure query_netchanger (
				name	: in type_netchanger_id;
				nc		: in out type_netchanger)
			is begin
				modify_status (nc, operation);
				-- log (text => "done", level => log_threshold + 1);

				-- If the netchanger is set as moving, then
				-- backup the original position:
				if get_action (operation) = SET and
					get_flag (operation) = MOVING then

					object_original_position := get_place (nc);
				end if;
			end query_netchanger;

			
		begin
			module.netchangers.update_element (
				netchanger.netchanger_cursor, query_netchanger'access);
				
		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " modify status of netchanger "
			& get_object_name (netchanger)
			& " / " & to_string (operation),
			level => log_threshold);


		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;





	

	procedure propose_netchangers (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_netchangers;
			netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;

			
			procedure query_netchanger (
				name		: in type_netchanger_id;
				netchanger	: in out type_netchanger)
			is begin
				if in_catch_zone (netchanger, catch_zone) then
					log (text => to_string (name), level => log_threshold + 1);
					
					set_proposed (netchanger);
					count := count + 1;
				end if;
			end query_netchanger;

			
		begin
			-- Iterate through the netchangers:
			while has_element (netchanger_cursor) loop
				module.netchangers.update_element (
					netchanger_cursor, query_netchanger'access);
					
				next (netchanger_cursor);
			end loop;
		end query_module;


		
	begin
		log (text => "module " & to_string (module_cursor)
			& " propose netchangers in " & to_string (catch_zone),
			level => log_threshold);
		
		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end propose_netchangers;






	


	procedure reset_status_netchangers (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
		
			use pac_netchangers;
			netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;

			
			procedure query_netchanger (
				name		: in type_netchanger_id;
				netchanger	: in out type_netchanger)
			is begin
				log (text => to_string (name), level => log_threshold + 1);
				reset_status (netchanger);
			end query_netchanger;

			
		begin
			-- Iterate through the netchangers:
			while has_element (netchanger_cursor) loop
				module.netchangers.update_element (
					netchanger_cursor, query_netchanger'access);
					
				next (netchanger_cursor);
			end loop;
		end query_module;
		
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " reset status of all netchangers", 
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_status_netchangers;







	

	function get_first_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_netchanger
	is
		result : type_object_netchanger;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_netchangers;
			netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;

			proceed : boolean := true;

			
			procedure query_netchanger (
				name		: in type_netchanger_id;
				netchanger	: in type_netchanger)
			is 
			
				procedure set_result is begin
					log (text => " found " & to_string (name), level => log_threshold + 2);
					result.netchanger_cursor := netchanger_cursor;
					proceed := false; -- no further probing required
				end set_result;

			
			begin
				log (text => to_string (name), level => log_threshold + 1);
				case flag is
					when PROPOSED =>
						if is_proposed (netchanger) then
							set_result;
						end if;
	
					when SELECTED =>
						if is_selected (netchanger) then
							set_result;
						end if;
	
					when others => null; -- CS
				end case;
			end query_netchanger;

	
		begin
			-- Iterate through the netchangers:
			while has_element (netchanger_cursor) and proceed loop
				query_element (netchanger_cursor, query_netchanger'access);
				next (netchanger_cursor);
			end loop;

			if proceed then
				log (text => "nothing found", level => log_threshold);
			end if;
		end query_module;
		
		
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first netchanger / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_netchanger;








------------------------------------------------------------------------------------------

-- OBJECTS:
	

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
		result_netchanger	: type_object_netchanger;

		use pac_netchangers;
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;

		
		-- SEARCH FOR A NETCHANGER:
		
		-- If a netchanger has been found, then go to the end of this procedure:
		result_netchanger := get_first_netchanger (module_cursor, flag, log_threshold + 1);

		if has_element (result_netchanger.netchanger_cursor) then
			-- A netchanger has been found.
			log (text => get_object_name (result_netchanger),
				 level => log_threshold + 1);
			
			result_category := CAT_NETCHANGER;
		end if;
		
		-- If nothing has been found then the category is CAT_VOID.
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;




	<<end_of_search>>
		
		-- If nothing has been found then the category is CAT_VOID.
		log_indentation_down;

		
		
		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_NETCHANGER =>
				return (CAT_NETCHANGER, result_netchanger);
				
		end case;
	end get_first_object;

	
	



	
	



	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return pac_objects.list
	is
		use pac_objects;

		-- Here the objects are collected:
		result : pac_objects.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is

			-- This procedure queries the netchangers
			-- and collects those which have the given flag set:			
			procedure query_netchangers is
				use pac_netchangers;
				netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;
				

				-- Queries a unit for its status flag
				-- and appends it to the result:
				procedure query_netchanger (
					name		: in type_netchanger_id;
					netchanger	: in type_netchanger) 
				is 

					-- This procedure appends the matching
					-- netchanger to the result:
					procedure collect is begin
						log (text => to_string (name), level => log_threshold + 4);
						
						result.append ((
							cat			=> CAT_NETCHANGER,
							netchanger	=> (netchanger_cursor => netchanger_cursor)));

					end collect;
						
					
				begin
					log (text => to_string (name), level => log_threshold + 2);
					log_indentation_up;
					
					case flag is
						when PROPOSED =>
							if is_proposed (netchanger) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (netchanger) then
								collect;
							end if;

						when others => null; -- CS
					end case;					

					log_indentation_down;
				end query_netchanger;

				
			begin
				log (text => "query_netchangers", level => log_threshold + 1);
				log_indentation_up;
				
				-- Iterate the netchangers of the module:
				while has_element (netchanger_cursor) loop
					query_element (netchanger_cursor, query_netchanger'access);
					next (netchanger_cursor);
				end loop;

				log_indentation_down;
			end query_netchangers;
			
			
		begin
			query_netchangers;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up objects / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
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
			& " modify status of object "
			& type_object_category'image (object.cat)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		case object.cat is
			when CAT_NETCHANGER =>
				modify_status (module_cursor, object.netchanger,
					operation, log_threshold + 1);
			
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



	


	procedure reset_status_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is 

		procedure reset_devices is begin
			-- Reset netchangers:
			reset_status_netchangers (module_cursor, log_threshold + 1);
			-- CS notes, properties, ...
		end;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " reset objects",
			level => log_threshold);

		log_indentation_up;
		reset_devices;		
		log_indentation_down;
	end reset_status_objects;

	
	
	
	
	
	
	
	
	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " move object " 
			-- CS & to_string (object)
			& " to " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NETCHANGER =>

				move_netchanger (
					module_cursor	=> module_cursor,
					index			=> get_object_id (object.netchanger),
					coordinates		=> absolute,
					point			=> destination,
					log_threshold	=> log_threshold + 1);
				

			--when CAT_VOID =>
			when others =>
				null;
		end case;		
		
		log_indentation_down;
	end move_object;


	
	
	

end et_board_ops_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
