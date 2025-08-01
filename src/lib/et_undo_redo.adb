------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       UNDO / REDO OPERATIONS                             --
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
-- DESCRIPTION:
-- 

with ada.text_io;						use ada.text_io;
with ada.containers;					use ada.containers;

with et_domains;						use et_domains;
with et_nets;
with et_pcb;
with et_modes.schematic;
with et_modes.board;
with et_time;							use et_time;
with et_module_names;					use et_module_names;
with et_devices_electrical;				use et_devices_electrical;
with et_devices_non_electrical;			use et_devices_non_electrical;



package body et_undo_redo is
	
	
	procedure commit ( -- in schematic domain
		stage	: in type_commit_stage;
		verb	: in et_modes.schematic.type_verb;
		noun	: in et_modes.schematic.type_noun;
		lth		: in type_log_level)
	is
		use et_modes.schematic;

		domain : constant type_domain := DOM_SCHEMATIC;
		
		verb_noun : constant string := to_string (verb) & " " & to_string (noun);
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			use pac_commit_message;

			
			procedure commit_nets is 
				use et_nets;
			begin
				log (text => "nets", level => lth + 1);
				
				module.net_commits.dos.append (pac_net_commit.make_commit (
					index	=> module.commit_index, 
					stage	=> stage, 
					item	=> module.nets,
					message	=> to_bounded_string (verb_noun),
					domain	=> domain));
			end commit_nets;

			
			procedure commit_devices is begin
				log (text => "devices", level => lth + 1);
				
				module.device_commits.dos.append (pac_device_commit.make_commit (
					index	=> module.commit_index, 
					stage	=> stage, 
					item	=> module.devices,
					message	=> to_bounded_string (verb_noun),
					domain	=> domain));

			end commit_devices;

			
		begin -- query_module
			increment (module.commit_index);			

			case noun is
				when NOUN_SEGMENT | NOUN_STRAND =>
					case verb is
						when VERB_DELETE =>
							commit_nets;

						when others => null;
					end case;


				when NOUN_NET =>
					case verb is
						when VERB_DRAW | VERB_DELETE | VERB_DRAG =>
							commit_nets;

						when others => null;
					end case;
					
				when NOUN_UNIT =>
					case verb is
						when VERB_FETCH | VERB_MOVE | VERB_DELETE | VERB_DRAG =>
							commit_devices;
							commit_nets;

						when others => null;
					end case;

				when others => null;
			end case;
		end query_module;
		

	begin
		log (text => "commit in " & to_string (domain) & " (" 
			 & to_string (stage) & " / " & verb_noun & ")",
			 level => lth + 1);
		
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> active_module,
			process		=> query_module'access);

		log_indentation_down;
	end commit;





	

	procedure commit ( -- in board domain
		stage	: in type_commit_stage;
		verb	: in et_modes.board.type_verb;
		noun	: in et_modes.board.type_noun;
		lth		: in type_log_level)
	is
		use et_modes.board;

		domain : constant type_domain := DOM_BOARD;

		verb_noun : constant string := to_string (verb) & " " & to_string (noun);

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			use pac_commit_message;

			
			procedure commit_nets is 
				use et_nets;
			begin
				log (text => "nets", level => lth + 1);
							
				module.net_commits.dos.append (pac_net_commit.make_commit (
					index	=> module.commit_index, 
					stage	=> stage, 
					item	=> module.nets,
					message	=> to_bounded_string (verb_noun),
					domain	=> domain));
					-- CS In order to save memory, do not commit the fill lines of zones ?
			end commit_nets;

			
			procedure commit_devices is begin
				log (text => "devices", level => lth + 1);
							
				module.device_commits.dos.append (pac_device_commit.make_commit (
					index	=> module.commit_index, 
					stage	=> stage, 
					item	=> module.devices,
					message	=> to_bounded_string (verb_noun),
					domain	=> domain));
			end commit_devices;


			procedure commit_non_electrical_devices is 
				use et_pcb;
			begin
				log (text => "devices (non-electrical)", level => lth + 1);
							
				module.devices_non_electric_commits.dos.append (
					pac_non_electrical_device_commit.make_commit (
						index	=> module.commit_index, 
						stage	=> stage, 
						item	=> module.devices_non_electric,
						message	=> to_bounded_string (verb_noun),
						domain	=> domain));
			end commit_non_electrical_devices;


			procedure commit_board is 
				use et_pcb;
			begin
				log (text => "board objects (non-electrical)", level => lth + 1);
							
				module.board_commits.dos.append (
					pac_board_commit.make_commit (
						index	=> module.commit_index, 
						stage	=> stage, 
						item	=> module.board,
						message	=> to_bounded_string (verb_noun),
						domain	=> domain));
			end commit_board;

			
		begin -- query_module
			increment (module.commit_index);	

			log (text => "commit index " 
				 & type_commit_index_zero_based'image (module.commit_index),
				 level => lth + 1);
			
			case noun is
				when NOUN_NET =>
					case verb is
						when VERB_ROUTE =>
							commit_nets;
			
						when others =>
							null;
					end case;


				when NOUN_TRACK =>
					case verb is
						when VERB_MOVE | VERB_DELETE =>
							commit_nets;
			
						when others =>
							null;
					end case;


				when NOUN_DEVICE =>
					case verb is
						when VERB_MOVE | VERB_FLIP | VERB_ROTATE =>
							commit_devices;
			
						when others =>
							null;
					end case;


				when NOUN_NON_ELECTRICAL_DEVICE =>
					case verb is
						when VERB_MOVE | VERB_DELETE | VERB_ROTATE | VERB_ADD | VERB_FLIP =>
							commit_non_electrical_devices;
			
						when others =>
							null;
					end case;


				when NOUN_SILKSCREEN | NOUN_ASSY | NOUN_STOPMASK | NOUN_ROUTE_RESTRICT |
					NOUN_KEEPOUT | NOUN_VIA_RESTRICT | NOUN_LINE | NOUN_ARC | NOUN_TEXT => -- CS others ?
					case verb is
						when VERB_DRAW | VERB_MOVE | VERB_DELETE =>
							commit_board;
			
						when others =>
							null;
					end case;

					
				when others =>
					null;
			end case;
		end query_module;
		

	begin
		log (text => "commit in " & to_string (domain) & " (" 
			 & to_string (stage) & " / " & verb_noun & ")",
			 level => lth + 1);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> active_module,
			process		=> query_module'access);

		log_indentation_down;
	end commit;



	


	
	procedure undo (
		message	: in out pac_undo_message.bounded_string;
		lth		: in type_log_level)
	is
		use pac_undo_message;
		use pac_commit_message;

		domain : type_domain;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			-- After a successful redo-operation, this flag is set.
			done : boolean := false;

			nothing_to_do : constant string := "nothing to undo";

			
			procedure undo_nets is
				use et_nets;
				use pac_net_commits;
				dos		: pac_net_commits.list renames module.net_commits.dos;
				redos	: pac_net_commits.list renames module.net_commits.redos;

				-- Backup places for pre- and post-commits:
				pre_commit, post_commit : pac_net_commit.type_commit;
			begin
				if not dos.is_empty then
					if dos.last_element.index = module.commit_index then

						log (text => "nets", level => lth + 1);
						
						-- Backup post-commit and delete the original:
						post_commit := dos.last_element;
						dos.delete_last;

						-- Backup pre-commit and delete the original:
						pre_commit := dos.last_element;
						dos.delete_last;

						-- Restore the nets of the design according to the pre-commit:
						module.nets := pre_commit.item;

						-- Put pre- and post commit on redo-stack:
						redos.append (pre_commit);
						redos.append (post_commit);					

						-- Mark the undo-operation as successful:
						done := true;

						-- Add verb and noun to message:					
						message := to_bounded_string (to_string (post_commit.message));

						-- Add domain to message:
						domain := post_commit.domain;
					end if;
				end if;
			end undo_nets;

			

			procedure undo_devices is 
				use pac_device_commits;
				dos		: pac_device_commits.list renames module.device_commits.dos;
				redos	: pac_device_commits.list renames module.device_commits.redos;

				-- Backup places for pre- and post-commits:
				pre_commit, post_commit : pac_device_commit.type_commit;
			begin
				if not dos.is_empty then
					if dos.last_element.index = module.commit_index then

						log (text => "devices", level => lth + 1);
						
						-- Backup post-commit and delete the original:
						post_commit := dos.last_element;
						dos.delete_last;

						-- Backup pre-commit and delete the original:
						pre_commit := dos.last_element;
						dos.delete_last;

						-- Restore the devices of the design according to the pre-commit:
						module.devices := pre_commit.item;

						-- Put pre- and post commit on redo-stack:
						redos.append (pre_commit);
						redos.append (post_commit);					

						-- Mark the undo-operation as successful:
						done := true;

						-- Add verb and noun to message:
						message := to_bounded_string (to_string (post_commit.message));

						-- Add domain to message:
						domain := post_commit.domain;
					end if;
				end if;
			end undo_devices;


			
			procedure undo_non_electrical_devices is 
				use et_pcb;
				use pac_non_electrical_device_commits;
				dos		: pac_non_electrical_device_commits.list renames module.devices_non_electric_commits.dos;
				redos	: pac_non_electrical_device_commits.list renames module.devices_non_electric_commits.redos;

				-- Backup places for pre- and post-commits:
				pre_commit, post_commit : pac_non_electrical_device_commit.type_commit;
			begin
				if not dos.is_empty then
					if dos.last_element.index = module.commit_index then

						log (text => "devices (non-electrical)", level => lth + 1);
						
						-- Backup post-commit and delete the original:
						post_commit := dos.last_element;
						dos.delete_last;

						-- Backup pre-commit and delete the original:
						pre_commit := dos.last_element;
						dos.delete_last;

						-- Restore the devices of the design according to the pre-commit:
						module.devices_non_electric := pre_commit.item;

						-- Put pre- and post commit on redo-stack:
						redos.append (pre_commit);
						redos.append (post_commit);					

						-- Mark the undo-operation as successful:
						done := true;

						-- Add verb and noun to message:
						message := to_bounded_string (to_string (post_commit.message));

						-- Add domain to message:
						domain := post_commit.domain;
					end if;
				end if;
			end undo_non_electrical_devices;


			
			procedure undo_board is
				use et_pcb;
				use pac_board_commits;
				dos		: pac_board_commits.list renames module.board_commits.dos;
				redos	: pac_board_commits.list renames module.board_commits.redos;

				-- Backup places for pre- and post-commits:
				pre_commit, post_commit : pac_board_commit.type_commit;
			begin
				if not dos.is_empty then
					if dos.last_element.index = module.commit_index then

						log (text => "board objects (non-electrical)", level => lth + 1);
						
						-- Backup post-commit and delete the original:
						post_commit := dos.last_element;
						dos.delete_last;

						-- Backup pre-commit and delete the original:
						pre_commit := dos.last_element;
						dos.delete_last;

						-- Restore the devices of the design according to the pre-commit:
						module.board := pre_commit.item;

						-- Put pre- and post commit on redo-stack:
						redos.append (pre_commit);
						redos.append (post_commit);					

						-- Mark the undo-operation as successful:
						done := true;

						-- Add verb and noun to message:
						message := to_bounded_string (to_string (post_commit.message));

						-- Add domain to message:
						domain := post_commit.domain;
					end if;
				end if;
			end undo_board;

			
		begin -- query_module
			
			-- An undo-operation is allowed if there have been
			-- commits in the past. Otherwise there would be nothing to undo:
			if module.commit_index > 0 then

				-- Since we have multiple do-stacks (for various categories of objects),
				-- all the do-stacks that contain the latest commit must be processed.
				-- All other do-stacks remain untouched:

				-- Search in nets:
				undo_nets;				

				-- Search in devices:
				undo_devices;
				undo_non_electrical_devices;

				-- Search in board objects:
				undo_board;
				

				if done then
 					-- Add domain to message:
					message := message & to_bounded_string (" (in " & to_string (domain) & ")");
				
					-- Add preamble of undo-message:
					message := to_bounded_string ("undo: ") & message;
				end if;
				
				-- CS ? if done then
				decrement (module.commit_index, 2);

			else
				log (text => nothing_to_do, level => lth + 1);

				-- Assemble undo-message:
				message := to_bounded_string (nothing_to_do);
			end if;			
		end query_module;

		
	begin
		log (text => "undo", level => lth);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> active_module,
			process		=> query_module'access);

		log_indentation_down;
	end undo;
	

	


	

	procedure redo (
		message	: in out pac_redo_message.bounded_string;
		lth		: in type_log_level)
	is
		use pac_redo_message;
		use pac_commit_message;

		domain : type_domain;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 		
			-- Contains the index of the latest commit:
			commit_index : constant type_commit_index := module.commit_index + 2;

			-- After a successful redo-operation, this flag is set.
			done : boolean := false;

			nothing_to_do : constant string := "nothing to redo";
			
			
			procedure redo_nets is
				use et_nets;
				use pac_net_commits;				
				dos		: pac_net_commits.list renames module.net_commits.dos;
				redos	: pac_net_commits.list renames module.net_commits.redos;

				-- Backup places for pre- and post-commits:
				pre_commit, post_commit : pac_net_commit.type_commit;
			begin
				-- If there are no commits on the redo-stack, then there is nothing to do.
				if not redos.is_empty then

					-- Do the redo-operation if the last commit is 
					-- the latest among all commits:
					if redos.last_element.index = commit_index then

						log (text => "nets", level => lth + 1);
						
						-- Backup post-commit and delete the original:
						post_commit := redos.last_element;
						redos.delete_last;

						-- Backup pre-commit and delete the original:
						pre_commit := redos.last_element;
						redos.delete_last;

						
						-- Put pre- and post-commit back to dos-stack:
						dos.append (pre_commit);
						dos.append (post_commit);

						-- Restore design according to the post-commit:
						module.nets := post_commit.item;

						-- Mark the redo-operation as successful:
						done := true;

						-- Add verb and noun to message:					
						message := to_bounded_string (to_string (post_commit.message));

						-- Add domain to message:
						domain := post_commit.domain;
					end if;
				end if;
			end redo_nets;
			

			
			procedure redo_devices is
				use pac_device_commits;				
				dos		: pac_device_commits.list renames module.device_commits.dos;
				redos	: pac_device_commits.list renames module.device_commits.redos;

				-- Backup places for pre- and post-commits:
				pre_commit, post_commit : pac_device_commit.type_commit;
			begin
				-- If there are no commits on the redo-stack, then there is nothing to do.
				if not redos.is_empty then

					-- Do the redo-operation if the last commit is 
					-- the latest among all commits:
					if redos.last_element.index = commit_index then

						log (text => "devices", level => lth + 1);
						
						-- Backup post-commit and delete the original:
						post_commit := redos.last_element;
						redos.delete_last;

						-- Backup pre-commit and delete the original:
						pre_commit := redos.last_element;
						redos.delete_last;

						
						-- Put pre- and post-commit back to dos-stack:
						dos.append (pre_commit);
						dos.append (post_commit);

						-- Restore design according to the post-commit:
						module.devices := post_commit.item;

						-- Mark the redo-operation as successful:
						done := true;

						-- Add verb and noun to message:					
						message := to_bounded_string (to_string (post_commit.message));

						-- Add domain to message:
						domain := post_commit.domain;
					end if;
				end if;
			end redo_devices;


			
			procedure redo_non_electrical_devices is
				use et_pcb;
				use pac_non_electrical_device_commits;
				dos		: pac_non_electrical_device_commits.list renames module.devices_non_electric_commits.dos;
				redos	: pac_non_electrical_device_commits.list renames module.devices_non_electric_commits.redos;

				-- Backup places for pre- and post-commits:
				pre_commit, post_commit : pac_non_electrical_device_commit.type_commit;				
			begin
				-- If there are no commits on the redo-stack, then there is nothing to do.
				if not redos.is_empty then

					-- Do the redo-operation if the last commit is 
					-- the latest among all commits:
					if redos.last_element.index = commit_index then

						log (text => "devices (non-electrical)", level => lth + 1);
						
						-- Backup post-commit and delete the original:
						post_commit := redos.last_element;
						redos.delete_last;

						-- Backup pre-commit and delete the original:
						pre_commit := redos.last_element;
						redos.delete_last;

						
						-- Put pre- and post-commit back to dos-stack:
						dos.append (pre_commit);
						dos.append (post_commit);

						-- Restore design according to the post-commit:
						module.devices_non_electric := post_commit.item;

						-- Mark the redo-operation as successful:
						done := true;

						-- Add verb and noun to message:					
						message := to_bounded_string (to_string (post_commit.message));

						-- Add domain to message:
						domain := post_commit.domain;
					end if;
				end if;
			end redo_non_electrical_devices;

			

			procedure redo_board is
				use et_pcb;
				use pac_board_commits;				
				dos		: pac_board_commits.list renames module.board_commits.dos;
				redos	: pac_board_commits.list renames module.board_commits.redos;

				-- Backup places for pre- and post-commits:
				pre_commit, post_commit : pac_board_commit.type_commit;
			begin
				-- If there are no commits on the redo-stack, then there is nothing to do.
				if not redos.is_empty then

					-- Do the redo-operation if the last commit is 
					-- the latest among all commits:
					if redos.last_element.index = commit_index then

						log (text => "board objects (non-electrical)", level => lth + 1);
						
						-- Backup post-commit and delete the original:
						post_commit := redos.last_element;
						redos.delete_last;

						-- Backup pre-commit and delete the original:
						pre_commit := redos.last_element;
						redos.delete_last;

						
						-- Put pre- and post-commit back to dos-stack:
						dos.append (pre_commit);
						dos.append (post_commit);

						-- Restore design according to the post-commit:
						module.board := post_commit.item;

						-- Mark the redo-operation as successful:
						done := true;

						-- Add verb and noun to message:					
						message := to_bounded_string (to_string (post_commit.message));

						-- Add domain to message:
						domain := post_commit.domain;
					end if;
				end if;
			end redo_board;

			
		begin -- query_module

			-- Since we have multiple redo-stacks (for various categories of objects),
			-- all the redo-stacks that contain the latest commit must be processed.
			-- All other redo-stacks remain untouched:
			
			-- Search in nets:
			redo_nets;

			-- Search in devices:
			redo_devices;
			redo_non_electrical_devices;

			-- Search in board objects:
			redo_board;
			

			if done then
				-- Add domain to message:
				message := message & to_bounded_string (" (in " & to_string (domain) & ")");
			
				-- Add preamble of message:
				message := to_bounded_string ("redo: ") & message;

				increment (module.commit_index, 2);
			else
				log (text => nothing_to_do, level => lth + 1);

				-- Assemble redo-message:
				message := to_bounded_string (nothing_to_do);
			end if;
		end query_module;

		
	begin
		log (text => "redo", level => lth);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> active_module,
			process		=> query_module'access);

		log_indentation_down;		
	end redo;

	
end et_undo_redo;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
