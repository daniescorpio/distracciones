with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts       
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    -----------------------------------------------------------------------
    ------------- constants 
    -----------------------------------------------------------------------
    
    LEER_POSICION_CABEZA_PRIORITY 	  : Integer   := 1;
    LEER_DISTANCIA_PRIORITY 	  	  : Integer   := 2;
    MOSTRAR_INFO_DISPLAY_PRIORITY  	  : Integer   := 3;
    CALCULAR_RIESGOS_PRIORITY	  	  : Integer   := 4;
    LEER_GIRO_VOLANTE_PRIORITY 	  	  : Integer   := 5;
    INTERVALO_LECTURA_POSICION_CABEZA     : Time_Span := Milliseconds (400);
    INTERVALO_LECTURA_DISTANCIA_SEGURIDAD : Time_Span := Milliseconds (300);
    INTERVALO_LECTURA_GIRO_VOLANTE        : Time_Span := Milliseconds (350);
    INTERVALO_DETECCION_RIESGOS		  : Time_Span := Milliseconds (150);
    INTERVALO_REFRESCO_DISPLAY		  : Time_Span := Milliseconds (1000);
    
    
    -----------------------------------------------------------------------
    ------------- custom types 
    -----------------------------------------------------------------------
    
    type Distraccion_Estado_Type is (ESTADO_DISTRACCION, ESTADO_NO_DISTRACCION);
    
    
    -----------------------------------------------------------------------
    ------------- declaration of protected objects 
    -----------------------------------------------------------------------

    Protected Sintomas is
      procedure Guardar_Estado_Distraccion (actual_Distraccion: in Distraccion_Estado_Type);
      procedure Volantazo;
      function Obtener_Estado_Distraccion return Distraccion_Estado_Type;
    private
      Distraccion : Distraccion_Estado_Type;
      
    end Sintomas;
    
    --Protected Medidas is
      --function LeerSensorCabeza return HeadPosition_Samples_Type;
    --private
    --end Medidas;
    
    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    -- Aqui se declaran las tareas que forman el STR
    
    task LeerPosicionCabeza is 
      pragma priority (LEER_POSICION_CABEZA_PRIORITY);
    end leerPosicionCabeza;
    
    task LeerDistancia is 
      pragma priority (LEER_DISTANCIA_PRIORITY);
    end LeerDistancia;
    
    task MostrarInfoDisplay is
      pragma priority (MOSTRAR_INFO_DISPLAY_PRIORITY);
    end; 
    
    task CalcularRiesgos is
      pragma priority (CALCULAR_RIESGOS_PRIORITY);
    end;
    
    task LeerGiroVolante is
      pragma priority (LEER_GIRO_VOLANTE_PRIORITY);
    end leerGiroVolante;


    -----------------------------------------------------------------------
    ------------- body of tasks 
    -----------------------------------------------------------------------
    
    -----------------------------------------------------------------------
    ------------- TAREA POSICION CABEZA 
    -----------------------------------------------------------------------
    
    task body LeerPosicionCabeza is 
      Current_H  	 : HeadPosition_Samples_Type;
      Previous_H 	 : HeadPosition_Samples_Type := (0, 0);
      Distraccion 	 : Distraccion_Estado_Type;
      Siguiente_Instante : Time;
      
    begin
    
      Siguiente_instante := Clock + INTERVALO_LECTURA_POSICION_CABEZA;
      Starting_Notice ("Prueba sensor cabeza");
      
      loop
      
        Distraccion := ESTADO_NO_DISTRACCION;
      
        Reading_HeadPosition(Current_H);
        
        if (Current_H(x) > 30 AND Previous_H(x) > 30) then 
          Distraccion := ESTADO_DISTRACCION;
        end if;
        
        if (Current_H(x) < -30 AND Previous_H(x) < -30) then
          Distraccion := ESTADO_DISTRACCION;
        end if;
        
        if (Current_H(y) > 30 AND Previous_H(y) > 30) then 
          Distraccion := ESTADO_DISTRACCION;
        end if;
        
        if (Current_H(y) < -30 AND Previous_H(y) < -30) then
          Distraccion := ESTADO_DISTRACCION;
        end if;
        
        Previous_H := Current_H;
        Sintomas.Guardar_Estado_Distraccion(Distraccion);
        Distraccion := ESTADO_NO_DISTRACCION;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_POSICION_CABEZA;
        
      end loop;
      
      Finishing_Notice ("Prueba sensor cabeza");
      
    end leerPosicionCabeza;
    
    
    
    -----------------------------------------------------------------------
    ------------- TAREA DISTANCIA DE SEGURIDAD 
    -----------------------------------------------------------------------

     task body LeerDistancia is
       Current_D 	   : Distance_Samples_Type := 0;
       Current_V 	   : Speed_Samples_Type    := 0;
       Distancia_Seguridad : Float		   := 0.0;
       Siguiente_Instante  : Time;
 
     begin
     
       Siguiente_instante := Clock + INTERVALO_LECTURA_DISTANCIA_SEGURIDAD;
       Starting_Notice ("Prueba sensor distancia");
	
       loop
      
         Reading_Distance (Current_D);
         --Display_Distance (Current_D);
      
         Reading_Speed (Current_V);
         Display_Speed (Current_V);
	 Distancia_Seguridad:= (Float(Current_V) / 10.0) ** 2;
        
         if (Float(Current_D) > Distancia_Seguridad) then
           Light(On);
         elsif(Float(Current_D) > Distancia_Seguridad / 2.0) then
	   Beep (4);
	   Light(On);
	 elsif(Float(Current_D) > Distancia_Seguridad / 3.0 ) then
	   Beep(5);
	 end if;
		
	 delay until Siguiente_Instante;
         Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_DISTANCIA_SEGURIDAD;
        
        end loop;
      
    	Finishing_Notice ("Prueba sensor distancia");
      
    end leerDistancia;
    
 
    -----------------------------------------------------------------------
    ------------- TAREA MOSTRAR INFO DISPLAY 
    -----------------------------------------------------------------------
        
    task body MostrarInfoDisplay is
      Distraccion 	 : Distraccion_Estado_Type;
      Siguiente_Instante : Time;
      
    begin
      
      Siguiente_instante := Clock + INTERVALO_REFRESCO_DISPLAY;
      Starting_Notice ("Prueba display");
      
      loop
        
        Distraccion := Sintomas.Obtener_Estado_Distraccion;
        
        if (Distraccion = ESTADO_DISTRACCION) then
          Current_Time (Big_Bang);
   	  Put ("............%");
          Put_Line ("¡¡¡ DISTRACCION DETECTADA !!!");
        end if;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_REFRESCO_DISPLAY;
        
      end loop;
      
      Finishing_Notice ("Prueba display");
      
    end;
    
    
    -----------------------------------------------------------------------
    ------------- TAREA DETECCION DE RIESGOS 
    -----------------------------------------------------------------------
        
    task body CalcularRiesgos is
      Distraccion 	 : Distraccion_Estado_Type;
      Siguiente_Instante : Time;
      
    begin
      
      Siguiente_instante := Clock + INTERVALO_DETECCION_RIESGOS;
      Starting_Notice ("Prueba deteccion de riesgos");
      
      loop
        
        Distraccion := Sintomas.Obtener_Estado_Distraccion;
        if (Distraccion = ESTADO_DISTRACCION) then 
          Beep(2);
          New_Line;
        end if;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_DETECCION_RIESGOS;
        
      end loop;
      
      Finishing_Notice ("Prueba deteccion de riesgos");
      
    end;
    
    
    -----------------------------------------------------------------------
    ------------- TAREA GIRO VOLANTE 
    -----------------------------------------------------------------------
    
    task body LeerGiroVolante is
    
      Current_S     	 : Steering_Samples_Type;
      Previous_S    	 : Steering_Samples_Type := 0;
      Actual_Offset 	 : Steering_Samples_Type;
      Siguiente_Instante : Time;
      
    begin
      
      Siguiente_instante := Clock + INTERVALO_LECTURA_GIRO_VOLANTE;
      Starting_Notice ("Prueba sensor giro volante");
      
      loop
        
        Reading_Steering (Current_S);
        
        Actual_Offset := Current_S - Previous_S;
        
        --if (Actual_Offset > 20 OR Actual_Offset < -20) then 
          --Sintoma de volantazo
          --Display_Steering (Current_S);
          --Put_Line (" --> VOLANTAZO");
        --end if;
        
        Previous_S := Current_S;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_GIRO_VOLANTE;
        
      end loop;
      
      Finishing_Notice ("Prueba sensor giro volante");
      
    end leerGiroVolante;
    
    
    -----------------------------------------------------------------------
    ------------- body of protected objects 
    -----------------------------------------------------------------------
    
    protected body Sintomas is
      
      procedure Guardar_Estado_Distraccion(actual_Distraccion: in Distraccion_Estado_Type) is
      begin
        Distraccion := actual_Distraccion;
      end Guardar_Estado_Distraccion;
      
      function Obtener_Estado_Distraccion return Distraccion_Estado_Type is
      begin 
        return Distraccion;
      end Obtener_Estado_Distraccion;
      
      procedure Volantazo is
        s : Steering_Samples_Type;
      begin
        Reading_Steering (s);
      end Volantazo;
      
    end Sintomas;
    
    
    --protected body Medidas is
    
      --function LeerSensorCabeza return HeadPosition_Samples_Type is
        --h : HeadPosition_Samples_Type;
      --begin
        --Reading_HeadPosition(h);
        --return h;
      --end LeerSensorCabeza;
      
    --end Medidas;
    
   
    ----------------------------------------------------------------------
    ------------- procedure para probar los dispositivos 
    ----------------------------------------------------------------------
    procedure Prueba_Dispositivos; 

    Procedure Prueba_Dispositivos is
        Current_V : Speed_Samples_Type := 0;
        Current_H : HeadPosition_Samples_Type := (+2,-2);
        Current_D : Distance_Samples_Type := 0;
        Current_O : Eyes_Samples_Type := (70,70);
        Current_E : EEG_Samples_Type := (1,1,1,1,1,1,1,1,1,1);
        Current_S : Steering_Samples_Type := 0;
    begin
         --Starting_Notice ("Prueba_Dispositivo");

         --for I in 1..120 loop
         -- Prueba distancia
            --Reading_Distance (Current_D);
            --Display_Distance (Current_D);
            --if (Current_D < 40) then Light (On); 
            --                    else Light (Off); end if;

         -- Prueba velocidad
            --Reading_Speed (Current_V);
            --Display_Speed (Current_V);
            --if (Current_V > 110) then Beep (2); end if;

         -- Prueba volante
            --Reading_Steering (Current_S);
            --Display_Steering (Current_S);
            --if (Current_S > 30) OR (Current_S < -30) then Light (On);
            --                                         else Light (Off); end if;

         -- Prueba Posicion de la cabeza
            --Reading_HeadPosition (Current_H);
            --Display_HeadPosition_Sample (Current_H);
            --if (Current_H(x) > 30) then Beep (4); end if;

         -- Prueba ojos
            --Reading_EyesImage (Current_O);
            --Display_Eyes_Sample (Current_O);

         -- Prueba electroencefalograma
            --Reading_Sensors (Current_E);
            --Display_Electrodes_Sample (Current_E);
   
         --delay until (Clock + To_time_Span(0.1));
         --end loop;

         --Finishing_Notice ("Prueba_Dispositivo");
         null;
    end Prueba_Dispositivos;


begin
   null;
   --Starting_Notice ("Programa Principal");
   --Prueba_Dispositivos;
   --Finishing_Notice ("Programa Principal");
end add;



