------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / RATSNEST                          --
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
--   ToDo: 

with ada.exceptions;					use ada.exceptions;

with et_conductor_segment.boards;
with et_board_ops_devices;				use et_board_ops_devices;
with et_board_ops.vias;					use et_board_ops.vias;


package body et_board_ops.ratsnest is

	
	use pac_nets;


	function get_track_ends (
		net_cursor : in pac_nets.cursor)
		return pac_points.list
	is
		use et_conductor_segment.boards;
		use pac_points;
		result : pac_points.list;

		use pac_conductor_lines;
		procedure query_line (l : in pac_conductor_lines.cursor) is
		begin
			append (result, get_A (l));
			append (result, get_B (l));
		end query_line;

		
		use pac_conductor_arcs;
		procedure query_arc (a : in pac_conductor_arcs.cursor) is
		begin
			append (result, get_A (a));
			append (result, get_B (a));
		end query_arc;

		
	begin
		-- Query lines and arc segments:
		iterate (element (net_cursor).route.lines, query_line'access);
		iterate (element (net_cursor).route.arcs, query_arc'access);

		-- The ends of segments frequently overlap with those of other
		-- segments. This causes redundant points which must be removed:
		remove_redundant_points (result);
		
		return result;
	end get_track_ends;


	
	
	procedure update_ratsnest (
		module_cursor	: in pac_generic_modules.cursor;
		lth				: in type_log_level)
	is
		use et_conductor_segment.boards;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is				
				use pac_geometry_brd;
				use pac_vectors;
				nodes : pac_vectors.list;
				
				use pac_airwires;
				airwires : pac_airwires.list;
				
				fragments : et_ratsnest.pac_isolated_fragments.list;
				
			begin -- query_net
				log (text => "net " & to_string (net_name), level => lth + 1);
				log_indentation_up;
				
				-- get x/y positions of all terminals:
				nodes := get_terminal_positions (
					module_cursor	=> module_cursor, 
					net_cursor		=> net_cursor,
					log_threshold	=> lth + 2);

				log (text => "get via positions", level => lth + 2);
				
				-- Get x/y of all vias and append their positions to nodes.
				-- The via positions must be converted to location vectors:
				splice_vectors (nodes, to_vectors (get_via_positions (net_cursor)));

				log (text => "get track ends", level => lth + 2);
				
				-- Get x/y of track segments (start and end points)
				-- and append their positions to nodes.
				-- The end points of the tracks must be converted to location vectors:
				splice_vectors (nodes, to_vectors (get_track_ends (net_cursor)));

				-- CS submodules ?
				
				-- remove redundant/overlapping nodes
				remove_redundant_vectors (nodes);
				
					
				-- COMPUTE THE RATSNEST / AIRWIRES
				
				-- Compute the isolated fragments formed by lines and arcs.
				-- NOTE: Vias and THT-terminals are not required here, because
				-- solely the start/end points of lines and arcs matter to obtain
				-- the nodes. The physical connection by vias or THT-terminals
				-- is irrelevant here:
				fragments := get_fragments (
					lines		=> net.route.lines,
					arcs		=> net.route.arcs);

				
				-- Make airwires from the list of nodes:
				-- The container "nodes" contains ALL nodes of both
				-- unrouted and routed stuff.
				-- "fragments" contains nodes which are already
				-- directly connected via tracks, vias and tht-terminals (not via ariwires):
				airwires := make_airwires (nodes, fragments);
				
				net.route.airwires.lines := airwires;

				log_indentation_down;
			end query_net;

			
		begin
			while net_cursor /= pac_nets.no_element loop
				module.nets.update_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;				
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " update ratsnest",
			level => lth);

		log_indentation_up;

		update_element (generic_modules, module_cursor, query_module'access);

		log_indentation_down;

		
		exception
			when event: others =>
				log (text => ada.exceptions.exception_information (event), console => true);
				--log (text => ada.exceptions.exception_information (event));

		
	end update_ratsnest;


	



	
	procedure propose_airwires (
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
				use pac_airwires;
				
				airwire_cursor : pac_airwires.cursor := net.route.airwires.lines.first;

				
				procedure query_airwire (wire : in out type_airwire) is
					w_tmp : type_line := type_line (to_line_coarse (wire));
					use pac_geometry_brd;
				begin
					if in_catch_zone (catch_zone, w_tmp) then
						set_proposed (wire);
						count := count + 1;
						log (text => to_string (wire), level => log_threshold + 2);
					end if;
				end query_airwire;

				
			begin
				log (text => "net " & to_string (net_name), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate the airwires:
				while has_element (airwire_cursor) loop
					net.route.airwires.lines.update_element (
						airwire_cursor, query_airwire'access);

					next (airwire_cursor);
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
		log (text => "propose airwires in " & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_airwires;





	procedure reset_proposed_airwires (
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
				use pac_airwires;
				
				airwire_cursor : pac_airwires.cursor := net.route.airwires.lines.first;

				
				procedure query_airwire (wire : in out type_airwire) is
					w_tmp : type_line := type_line (to_line_coarse (wire));
					use pac_geometry_brd;
				begin
					reset_status (wire);
				end query_airwire;

				
			begin
				log (text => "net " & to_string (net_name), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate the airwires:
				while has_element (airwire_cursor) loop
					net.route.airwires.lines.update_element (
						airwire_cursor, query_airwire'access);

					next (airwire_cursor);
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
		log (text => "reset proposed lines",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_airwires;




	function get_net_name (
		object : in pac_objects.cursor)
		return pac_net_name.bounded_string
	is 
		use pac_objects;
		use pac_nets;
		aw : type_object_airwire := element (object);
	begin
		return get_net_name (aw.net_cursor);
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
		return type_object_airwire
	is 
		result : type_object_airwire;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			proceed : aliased boolean := true;


			procedure query_net (net_cursor : in pac_nets.cursor) is

				procedure query_airwires (
					net_name	: in pac_net_name.bounded_string;
					net 		: in type_net)
				is 

					procedure query_airwire (w : in pac_airwires.cursor) is begin
						case flag is
							when PROPOSED =>
								if is_proposed (w) then
									result.net_cursor := net_cursor;
									result.wire_cursor := w;
									proceed := false;  -- no further probing required
									log (text => to_string (w), level => log_threshold + 2);
								end if;
      
							when SELECTED =>
								if is_selected (w) then
									result.net_cursor := net_cursor;
									result.wire_cursor := w;
									proceed := false;  -- no further probing required
									log (text => to_string (w), level => log_threshold + 2);
								end if;
      
							when others =>
								null; -- CS
						end case;
					end query_airwire;


				begin
					iterate (net.route.airwires.lines, query_airwire'access, proceed'access);
				end query_airwires;
				
				
			begin
				log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
				log_indentation_up;
				query_element (net_cursor, query_airwires'access);
				log_indentation_down;
			end query_net;
				

		begin
			iterate (module.nets, query_net'access, proceed'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " look up the first airwire / " & to_string (flag),
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

				procedure query_airwires (
					net_name	: in pac_net_name.bounded_string;
					net 		: in type_net)
				is 

					procedure query_airwire (w : in pac_airwires.cursor) is begin
						case flag is
							when PROPOSED =>
								if is_proposed (w) then
									result.append ((w, net_cursor));
									log (text => to_string (w), level => log_threshold + 2);
								end if;
      
							when SELECTED =>
								if is_selected (w) then
									result.append ((w, net_cursor));
									log (text => to_string (w), level => log_threshold + 2);
								end if;
      
							when others =>
								null; -- CS
						end case;
					end query_airwire;


				begin
					iterate (net.route.airwires.lines, query_airwire'access, proceed'access);
				end query_airwires;
				
				
			begin
				log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
				log_indentation_up;
				query_element (net_cursor, query_airwires'access);
				log_indentation_down;
			end query_net;
				

		begin
			iterate (module.nets, query_net'access, proceed'access);
		end query_module;
		
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " look up airwires / " & to_string (flag),
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
		object			: in type_object_airwire;
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
				use pac_airwires;

				procedure query_airwire (w : in out type_airwire) is 
					use pac_geometry_brd;
				begin
					modify_status (w, operation);
				end query_airwire;

				
			begin
				net.route.airwires.lines.update_element (
					object.wire_cursor, query_airwire'access);

			end query_net;
			
			
			
		begin
			update_element (
				container	=> module.nets,
				position	=> object.net_cursor,
				process		=> query_net'access);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modify status of airwire "
			& to_string (object.wire_cursor)
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
		object : constant type_object_airwire := element (object_cursor);
	begin
		modify_status (module_cursor, object, operation, log_threshold);
	end modify_status;
	
	
											
end et_board_ops.ratsnest;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
