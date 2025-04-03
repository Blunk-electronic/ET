------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        BOARD OPERATIONS / VIAS                           --
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


with et_schematic_ops;				use et_schematic_ops;
with et_schematic_ops.nets;			use et_schematic_ops.nets;

with et_board_ops.ratsnest;			use et_board_ops.ratsnest;
with et_keywords;					use et_keywords;


package body et_board_ops.vias is

	use pac_generic_modules;
	use pac_nets;
	use pac_net_name;
	use pac_vias;	


	
	function get_via_positions (
		net_cursor : in pac_nets.cursor)
		return pac_points.list
	is
		use pac_points;
		result : pac_points.list;

		use pac_vias;
		procedure query_via (v : in pac_vias.cursor) is begin
			append (result, element (v).position);
		end query_via;
		
	begin
		iterate (element (net_cursor).route.vias, query_via'access);
		return result;
	end get_via_positions;
	


	

	function to_string (
		via	: in pac_proposed_vias.cursor)
		return string
	is
		use pac_proposed_vias;
		v : type_via renames element (via).via;
		n : pac_net_name.bounded_string renames element (via).net;
	begin
		return to_string (v.position) & ". Cat " 
			& to_string (v.category) & ". Net " & to_string (n);
	end to_string;


	

	
	function get_vias (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_proposed_vias.list
	is
		use pac_proposed_vias;
		result : pac_proposed_vias.list;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				name	: in pac_net_name.bounded_string;
				net		: in type_net) 
			is
				via_cursor : pac_vias.cursor := net.route.vias.first;

				
				procedure query_via (via : in type_via) is begin
					if in_catch_zone (
						zone	=> catch_zone,
						point	=> via.position)
					then
						log (text => to_string (via.position) 
							& " cat " & to_string (via.category)
							& " net " & to_string (name), 
							level => log_threshold + 2);
						
						result.append ((via.category, via, name));
					end if;
				end query_via;

				
			begin
				while via_cursor /= pac_vias.no_element loop
					query_element (via_cursor, query_via'access);
					next (via_cursor);
				end loop;
			end query_net;

			
		begin
			while net_cursor /= pac_nets.no_element loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;


	begin
		log (text => "looking up vias in " 
			& to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);


		log (text => "found" & count_type'image (result.length),
			 level => log_threshold + 1);
		
		log_indentation_down;		
		return result;
	end get_vias;






	procedure propose_vias (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				use et_nets;
				use pac_vias;
				
				via_cursor : pac_vias.cursor := net.route.vias.first;

				
				procedure query_via (via : in out type_via) is
					use pac_geometry_brd;
				begin
					if in_catch_zone (catch_zone, via) then
						set_proposed (via);
						count := count + 1;
						log (text => to_string (via), level => log_threshold + 2);
					end if;
				end query_via;

				
			begin
				log (text => "net " & to_string (net_name), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate the vias:
				while has_element (via_cursor) loop
					net.route.vias.update_element (
						via_cursor, query_via'access);

					next (via_cursor);
				end loop;

				log_indentation_down;
			end query_net;


			net_cursor : pac_nets.cursor := module.nets.first;
		begin
			-- Iterate the nets:
			while net_cursor /= pac_nets.no_element loop
				module.nets.update_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;
		
		
	begin
		log (text => "proposing vias in" & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_vias;


	


	procedure reset_proposed_vias (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				use et_nets;
				use pac_vias;
				
				via_cursor : pac_vias.cursor := net.route.vias.first;

				
				procedure query_via (via : in out type_via) is
					use pac_geometry_brd;
				begin
					reset_status (via);
				end query_via;

				
			begin
				log (text => "net " & to_string (net_name), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate the airwires:
				while has_element (via_cursor) loop
					net.route.vias.update_element (via_cursor, query_via'access);
					next (via_cursor);
				end loop;

				log_indentation_down;
			end query_net;


			net_cursor : pac_nets.cursor := module.nets.first;
		begin
			-- Iterate the nets:
			while net_cursor /= pac_nets.no_element loop
				module.nets.update_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "resetting proposed vias",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_vias;





	function get_net_name (
		object : in pac_objects.cursor)
		return pac_net_name.bounded_string
	is 
		use pac_objects;
		use pac_vias;
		v : type_object_via := element (object);
	begin
		return get_net_name (v.net_cursor);
	end get_net_name;


	

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
		return type_object_via
	is 
		result : type_object_via;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			proceed : aliased boolean := true;


			procedure query_net (net_cursor : in pac_nets.cursor) is

				procedure query_vias (
					net_name	: in pac_net_name.bounded_string;
					net 		: in type_net)
				is 

					procedure query_via (v : in pac_vias.cursor) is begin
						case flag is
							when PROPOSED =>
								if is_proposed (v) then
									result.net_cursor := net_cursor;
									result.via_cursor := v;
									proceed := false;  -- no further probing required
									log (text => to_string (v), level => log_threshold + 2);
								end if;
      
							when SELECTED =>
								if is_selected (v) then
									result.net_cursor := net_cursor;
									result.via_cursor := v;
									proceed := false;  -- no further probing required
									log (text => to_string (v), level => log_threshold + 2);
								end if;
      
							when others =>
								null; -- CS
						end case;
					end query_via;


				begin
					iterate (net.route.vias, query_via'access, proceed'access);
				end query_vias;
				
				
			begin
				log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
				log_indentation_up;
				query_element (net_cursor, query_vias'access);
				log_indentation_down;
			end query_net;
				

		begin
			iterate (module.nets, query_net'access, proceed'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first via / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
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
			module		: in type_generic_module) 
		is
			proceed : aliased boolean := true;


			procedure query_net (net_cursor : in pac_nets.cursor) is

				procedure query_vias (
					net_name	: in pac_net_name.bounded_string;
					net 		: in type_net)
				is 

					procedure query_via (v : in pac_vias.cursor) is begin
						case flag is
							when PROPOSED =>
								if is_proposed (v) then
									result.append ((v, net_cursor));
									log (text => to_string (v), level => log_threshold + 2);
								end if;
      
							when SELECTED =>
								if is_selected (v) then
									result.append ((v, net_cursor));
									log (text => to_string (v), level => log_threshold + 2);
								end if;
      
							when others =>
								null; -- CS
						end case;
					end query_via;


				begin
					iterate (net.route.vias, query_via'access, proceed'access);
				end query_vias;
				
				
			begin
				log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
				log_indentation_up;
				query_element (net_cursor, query_vias'access);
				log_indentation_down;
			end query_net;
				

		begin
			iterate (module.nets, query_net'access, proceed'access);
		end query_module;
		
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up vias / " & to_string (flag),
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
		object			: in type_object_via;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
  

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				use et_nets;
				use pac_vias;

				procedure query_via (v : in out type_via) is 
					use pac_geometry_brd;
				begin
					modify_status (v, operation);
				end query_via;

				
			begin
				net.route.vias.update_element (
					object.via_cursor, query_via'access);

			end query_net;
			
			
			
		begin
			update_element (
				container	=> module.nets,
				position	=> object.net_cursor,
				process		=> query_net'access);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of via "
			& to_string (object.via_cursor)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;
	



	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is 
		use pac_objects;
		object : constant type_object_via := element (object_cursor);
	begin
		modify_status (module_cursor, object, operation, log_threshold);
	end modify_status;


	
	


	
	
	procedure place_via (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		via				: in type_via;
		log_threshold	: in type_log_level) 
	is
		console : boolean := false; -- for test and debugging only

		procedure locate_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := find (module.nets, net_name);

			use et_nets;
			
			procedure locate_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				use pac_vias;
			begin
				append (
					container	=> net.route.vias,
					new_item	=> via);
			end locate_net;
			
		begin -- locate_module
			if net_exists (net_cursor) then

				pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> locate_net'access);
				
			else
				net_not_found (net_name);
			end if;
		end locate_module;

		
	begin -- place_via
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " placing via in net " & to_string (net_name) 
			& " at" & to_string (via.position)
			& " drill size " & to_string (via.diameter)
			& " cat " & to_string (via.category),
			console => console,
			level => log_threshold);

		case via.category is
			when THROUGH =>
				log (text => keyword_restring_inner & space
					 & to_string (via.restring_inner), 
					console => console,
					level => log_threshold);

				log (text => keyword_restring_outer & space
					 & to_string (via.restring_outer),
					console => console,
					level => log_threshold);

				
			when BLIND_DRILLED_FROM_TOP =>
				log (text => keyword_destination & space
					 & to_string (via.lower),
					console => console,
					level => log_threshold);

				log (text => keyword_restring_inner & space
					 & to_string (via.restring_inner),
					console => console,
					level => log_threshold);
				
				log (text => keyword_restring_outer & space
					 & to_string (via.restring_top), 
					console => console,
					level => log_threshold);


			when BLIND_DRILLED_FROM_BOTTOM =>
				log (text => keyword_destination & space
					 & to_string (via.upper), 
					console => console,
					level => log_threshold);

				log (text => keyword_restring_inner & space
					 & to_string (via.restring_inner),
					console => console,
					level => log_threshold);
				
				log (text => keyword_restring_outer & space
					 & to_string (via.restring_bottom),
					console => console,
					level => log_threshold);

				
			when BURIED =>
				log (text => keyword_layers & space
					 & to_string (via.layers),
					console => console,
					level => log_threshold);

				log (text => keyword_restring_inner & space
					 & to_string (via.restring_inner),
					console => console,
					level => log_threshold);

		end case;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> locate_module'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end place_via;


	
	function get_net (
		module_cursor	: in pac_generic_modules.cursor;
		via				: in type_via)
		return pac_net_name.bounded_string
	is
		result : pac_net_name.bounded_string;

		module : type_generic_module renames element (module_cursor);

		proceed : aliased boolean := true;

		procedure query_net (n : in pac_nets.cursor) is
			net : type_net renames element (n);

			procedure query_via (v : in pac_vias.cursor) is begin
				if element (v) = via then
					proceed := false;
					result := key (n);
				end if;
			end query_via;
			
		begin
			--put_line ("net " & to_string (key (n)));
			iterate (net.route.vias, query_via'access, proceed'access);
		end query_net;
		
	begin
		iterate (module.nets, query_net'access, proceed'access);
		return result;
	end get_net;	

	

	procedure move_via (
		module_cursor	: in pac_generic_modules.cursor;
		via				: in type_proposed_via;
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level)
	is
		new_position : type_vector_model := via.via.position;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			net_cursor : pac_nets.cursor := module.nets.find (via.net);
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				via_cursor : pac_vias.cursor := net.route.vias.find (via.via);
				
				procedure query_via (v : in out type_via) is begin
					v.position := new_position;
				end query_via;
				
			begin
				net.route.vias.update_element (via_cursor, query_via'access);
			end query_net;
			
		begin
			module.nets.update_element (net_cursor, query_net'access);
		end query_module;


	begin -- move_via

		case coordinates is
			when ABSOLUTE =>
				new_position := point;

			when RELATIVE =>
				move_by (new_position, to_distance_relative (point));
		end case;
		
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " net " & to_string (via.net)
			& " moving via from" & to_string (via.via.position)
			& " to" & to_string (new_position),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		update_ratsnest (module_cursor, log_threshold + 1);		
	end move_via;



	procedure delete_via (
		module_cursor	: in pac_generic_modules.cursor;
		via				: in type_proposed_via;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			net_cursor : pac_nets.cursor := module.nets.find (via.net);
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				via_cursor : pac_vias.cursor := net.route.vias.find (via.via);
			begin
				net.route.vias.delete (via_cursor);
			end query_net;
			
		begin
			module.nets.update_element (net_cursor, query_net'access);
		end query_module;

		
	begin

		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " net " & to_string (via.net)
			& " deleting via at" & to_string (via.via.position),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		update_ratsnest (module_cursor, log_threshold + 1);		
	end delete_via;

	
	
end et_board_ops.vias;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
