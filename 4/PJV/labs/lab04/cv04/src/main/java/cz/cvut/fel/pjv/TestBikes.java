package cz.cvut.fel.pjv;

import cz.cvut.fel.pjv.model.*;
import cz.cvut.fel.pjv.services.*;
import cz.cvut.fel.pjv.utils.*;

public class TestBikes {

    /**
     * This method tests classes implemented in Ukol 1.
     */
    public static void testBikesDescription() {

        Bicycle bike01, bike02, bike03;

        bike01 = new Bicycle(20, 10, 1);
        bike02 = new MountainBike(20, 10, 5, "Dual");
        bike03 = new RoadBike(40, 20, 8, 23);

        bike01.printDescription();
        bike02.printDescription();
        bike03.printDescription();



        /*
        Vystup na konzoli:
        Bike is in gear 1 with a cadence of 20 and travelling at a speed of 10.

        Bike is in gear 5 with a cadence of 20 and travelling at a speed of 10.
        The MountainBike has Dual suspension.

        Bike is in gear 8 with a cadence of 40 and travelling at a speed of 20.
        The RoadBike has 23mm tires.
        */

    }

    /**
     * This method tests classes naively implemented in Ukol 2.
     */
    public static void testBikeServices() {
        Bicycle bicycle, mountainBike, roadBike;
        bicycle = new Bicycle(20, 10, 1);
        mountainBike = new MountainBike(20, 10, 5, "Dual");
        roadBike = new RoadBike(40, 20, 8, 23);

        BasicService basicService = new BasicService();
        MountainBikeService mountainBikeService = new MountainBikeService();
        RoadBikeService roadBikeService = new RoadBikeService();

        System.out.println("put bicycle into BasicService:");
        basicService.accept(bicycle);
        System.out.println("put mountainBike into BasicService:");
        basicService.accept(mountainBike);
        System.out.println("put roadBike into BasicService:");
        basicService.accept(roadBike);
        // Intuitivni ocekavani: basicService odmitne mountainBike a roadBike

        System.out.println("put mountainBike into mountainBikeService:");
        mountainBikeService.accept(mountainBike);
        System.out.println("put roadBike into roadBikeService:");
        roadBikeService.accept(roadBike);
        // Intuitivni ocekavani: mountainBike a roadBike budou prijaty k oprave
        // sluzbama mountainBike a roadBike


        //Realita je ale jina:
        /*
            Vystup na konzoli:
            put bicycle into BasicService:
            fixing Bicycle.
            put mountainBike into BasicService:
            fixing Bicycle.
            put roadBike into BasicService:
            fixing Bicycle.
            put mountainBike into mountainBikeService:
            fixing Bicycle.
            put roadBike into roadBikeService:
            fixing Bicycle.
        */
    }

    /**
     * This method tests the double dispatch fix described in Ukol 3.
     */
    public static void testDoubleDispatch() {
        Bicycle bicycle, mountainBike, roadBike;
        bicycle = new Bicycle(20, 10, 1);
        mountainBike = new MountainBike(20, 10, 5, "Dual");
        roadBike = new RoadBike(40, 20, 8, 23);

        BasicService basicService = new BasicService();
        MountainBikeService mountainBikeService = new MountainBikeService();
        RoadBikeService roadBikeService = new RoadBikeService();

        System.out.println("put bicycle into BasicService:");
        // pedagogicke pretypovani (technicky je k nicemu ale je to napoveda)
//        bicycle.visit((BicycleVisitable) basicService);
        bicycle.visit(basicService);
        System.out.println("put mountainBike into BasicService:");
        mountainBike.visit(basicService);
        System.out.println("put roadBike into BasicService:");
        roadBike.visit(basicService);

        System.out.println("put mountainBike into mountainBikeService:");
        mountainBike.visit(mountainBikeService);
        System.out.println("put roadBike into roadBikeService:");
        roadBike.visit(roadBikeService);


        /*
        vystup na konzoli:
            put bicycle into BasicService:
            fixing Bicycle.
            put mountainBike into BasicService:
            can't fixcz.cvut.pjv.seminars.cv04.model.MountainBike
            put roadBike into BasicService:
            can't fixcz.cvut.pjv.seminars.cv04.model.RoadBike
            put mountainBike into mountainBikeService:
            fixing MountainBikeService.
            put roadBike into roadBikeService:
            fixing RoadBike.
        */
    }

    /**
     * Tests the holder classes described in Ukol 4.
     */
    public static void testHolders() {
        /* ODKOMENTUJ
        Bicycle bicycle = new Bicycle(20, 10, 1);
        MountainBike mountainBike = new MountainBike(20, 10, 5, "Dual");
        RoadBike roadBike = new RoadBike(40, 20, 8, 23);

        BicycleHolder bicycleHolder = new BicycleHolder(bicycle);
        MountainBikeHolder mountainBikeHolder = new MountainBikeHolder(
                mountainBike);
        RoadBikeHolder roadBikeHolder = new RoadBikeHolder(roadBike);

        bicycleHolder.getBike().printDescription();
        mountainBikeHolder.getBike().printDescription();
        roadBikeHolder.getBike().printDescription();
        */


        /*
        Vystup:
        Bike is in gear 1 with a cadence of 20 and travelling at a speed of 10.

        Bike is in gear 5 with a cadence of 20 and travelling at a speed of 10.
        The MountainBike has Dual suspension.

        Bike is in gear 8 with a cadence of 40 and travelling at a speed of 20.
        The RoadBike has 23mm tires.
        */
    }

    /**
     * Tests implementation of HoldingCar class described in Ukol 5.
     * One change: In Ukol 5 the class is named Car but here we use HoldingCar
     * instead since class named Car is already implemented in this project.
     */
    public static void testHoldingCar() {
        /* ODKOMENTUJ
        Bicycle bike01, bike02, bike03;

        bike01 = new Bicycle(20, 10, 1);
        bike02 = new MountainBike(20, 10, 5, "Dual");
        bike03 = new RoadBike(40, 20, 8, 23);

        System.out.println("Single dispatch:");
        HoldingCar car1 = new HoldingCar();
        car1.accept(bike01);
        car1.accept(bike02);
        car1.accept(bike03);
        System.out.println(car1);

        System.out.println("Double dispatch:");
        HoldingCar car2 = new HoldingCar();
        // pedagogicke pretypovani :)
        bike01.visit((BicycleVisitable) car2);
        bike02.visit(car2);
        bike03.visit(car2);
        System.out.println(car2);
        */


        /*
        Vystup:
        Single dispatch:
        This car has:
         -holder BicycleHolder holding Bicycle
         -holder BicycleHolder holding MountainBike
         -holder BicycleHolder holding RoadBike
        Double dispatch:
        This car has:
         -holder BicycleHolder holding Bicycle
         -holder MountainBikeHolder holding MountainBike
         -holder RoadBikeHolder holding RoadBike
        */
    }

    /**
     * Tests AnyHolderCar described in Ukol 7.
     */
    public static void testAnyHolderCar() {
        /* ODKOMENTUJ
        Bicycle bike01 = new Bicycle(20, 10, 1);
        MountainBike bike02 = new MountainBike(20, 10, 5, "Dual");
        RoadBike bike03 = new RoadBike(40, 20, 8, 23);

        System.out.println("AnyHolder holding mountainBike:");
        AnyHolder anyHolder = new AnyHolder<MountainBike>(bike02);
        anyHolder.getItem().printDescription();
        anyHolder.printBikeDescription();

        System.out.println("Double dispatch:");
        AnyHolderCar car2 = new AnyHolderCar();
        bike01.visit(car2);
        bike02.visit(car2);
        bike03.visit(car2);
        System.out.println(car2);
        */


        /*
        Vystup:
        Bike is in gear 5 with a cadence of 20 and travelling at a speed of 10.
        The MountainBike has Dual suspension.

        Bike is in gear 5 with a cadence of 20 and travelling at a speed of 10.
        The MountainBike has Dual suspension.
        Double dispatch:
        This car has:
         -holder AnyHolder holding Bicycle
         -holder AnyHolder holding MountainBike
         -holder AnyHolder holding RoadBike
        */
    }

    public static void testColors() {
        /* ODKOMENTUJ
        BikeColor colorOfMyBike = BikeColor.GREEN;
        System.out.println("The best color is" + colorOfMyBike.getName());

        Bicycle myBike = new Bicycle(10, 20, 1, colorOfMyBike);
        Bicycle redBike = new Bicycle(10, 20, 1, BikeColor.RED);
        Bicycle blueBike = new Bicycle(10, 20, 1, BikeColor.BLUE);

        System.out.println("myBike is " + myBike.getColor().getName());
        System.out.println("redBike is " + redBike.getColor().getName());
        System.out.println("blueBike is " + blueBike.getColor().getName());
        */


        /*
        vystup:
        The best color isgreen
        myBike is green
        redBike is red
        blueBike is blue
        */
    }

    public static void main(String[] args) {
//        testBikesDescription();
//        testBikeServices();
        testDoubleDispatch();
//        testHolders();
//        testHoldingCar();
//        testAnyHolderCar();
//        testColors();

    }
}
