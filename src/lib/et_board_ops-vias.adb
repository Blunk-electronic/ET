------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        BOARD OPERATIONS / VIAS                           --
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

with et_nets;						use et_nets;
with et_schematic_ops;				use et_schematic_ops;
with et_schematic_ops.nets;			use et_schematic_ops.nets;


package body et_board_ops.vias is

	use pac_generic_modules;
	use pac_nets;
	use pac_vias;	


	
	function get_via_positions (
		net_cursor : in et_schematic.pac_nets.cursor)
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
		point			: in type_point;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_proposed_vias.list
	is
		use pac_proposed_vias;
		result : pac_proposed_vias.list;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				name	: in pac_net_name.bounded_string;
				net		: in type_net) 
			is
				via_cursor : pac_vias.cursor := net.route.vias.first;
				
				procedure query_via (via : in type_via) is begin
					if in_catch_zone (
						point_1		=> point, 
						catch_zone	=> catch_zone,
						point_2		=> via.position)
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
		log (text => "looking up vias at" & to_string (point) 
			 & " catch zone" & catch_zone_to_string (catch_zone),
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

	
	
	procedure place_via (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		via				: in type_via;
		log_threshold	: in type_log_level) 
	is
		console : boolean := false; -- for test and debugging only

		procedure locate_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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
				log (text => et_vias.keyword_layers & space
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

		module : type_module renames element (module_cursor);

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
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level)
	is
		new_position : type_point := via.via.position;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
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
			module		: in out type_module)
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
