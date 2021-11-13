﻿// <auto-generated />
using System;
using DDDSample1.Infrastructure;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Metadata;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace DDDNetCore.Migrations
{
    [DbContext(typeof(DDDSample1DbContext))]
    partial class DDDSample1DbContextModelSnapshot : ModelSnapshot
    {
        protected override void BuildModel(ModelBuilder modelBuilder)
        {
#pragma warning disable 612, 618
            modelBuilder
                .UseIdentityColumns()
                .HasAnnotation("Relational:MaxIdentifierLength", 128)
                .HasAnnotation("ProductVersion", "5.0.0");

            modelBuilder.Entity("DDDNetCore.Domain.ConnectionRequests.DirectRequest", b =>
                {
                    b.Property<string>("Id")
                        .HasColumnType("nvarchar(450)");

                    b.Property<bool>("Active")
                        .HasColumnType("bit");

                    b.Property<string>("Player")
                        .IsRequired()
                        .HasMaxLength(70)
                        .HasColumnType("nvarchar(70)");

                    b.Property<string>("Target")
                        .IsRequired()
                        .HasMaxLength(70)
                        .HasColumnType("nvarchar(70)");

                    b.HasKey("Id");

                    b.ToTable("DirectRequests");
                });

            modelBuilder.Entity("DDDNetCore.Domain.ConnectionRequests.IntroductionRequest", b =>
                {
                    b.Property<string>("Id")
                        .HasColumnType("nvarchar(450)");

                    b.Property<bool>("Active")
                        .HasColumnType("bit");

                    b.Property<string>("MiddleMan")
                        .IsRequired()
                        .HasMaxLength(70)
                        .HasColumnType("nvarchar(70)");

                    b.Property<string>("Player")
                        .IsRequired()
                        .HasMaxLength(70)
                        .HasColumnType("nvarchar(70)");

                    b.Property<string>("Target")
                        .IsRequired()
                        .HasMaxLength(70)
                        .HasColumnType("nvarchar(70)");

                    b.HasKey("Id");

                    b.ToTable("IntroductionRequests");
                });

            modelBuilder.Entity("DDDNetCore.Domain.Connections.Connection", b =>
                {
                    b.Property<string>("Id")
                        .HasColumnType("nvarchar(450)");

                    b.Property<bool>("Active")
                        .HasColumnType("bit");

                    b.Property<string>("Friend")
                        .IsRequired()
                        .HasMaxLength(70)
                        .HasColumnType("nvarchar(70)");

                    b.Property<string>("Player")
                        .IsRequired()
                        .HasMaxLength(70)
                        .HasColumnType("nvarchar(70)");

                    b.HasKey("Id");

                    b.ToTable("Connections");
                });

            modelBuilder.Entity("DDDNetCore.Domain.Missions.Mission", b =>
                {
                    b.Property<string>("Id")
                        .HasColumnType("nvarchar(450)");

                    b.Property<bool>("Active")
                        .HasColumnType("bit");

                    b.Property<string>("Challenger")
                        .IsRequired()
                        .HasMaxLength(70)
                        .HasColumnType("nvarchar(70)");

                    b.Property<string>("Objective")
                        .IsRequired()
                        .HasMaxLength(70)
                        .HasColumnType("nvarchar(70)");

                    b.HasKey("Id");

                    b.ToTable("Missions");
                });

            modelBuilder.Entity("DDDSample1.Domain.Categories.Category", b =>
                {
                    b.Property<string>("Id")
                        .HasColumnType("nvarchar(450)");

                    b.Property<bool>("Active")
                        .HasColumnType("bit");

                    b.Property<string>("Description")
                        .HasColumnType("nvarchar(max)");

                    b.HasKey("Id");

                    b.ToTable("Categories");
                });

            modelBuilder.Entity("DDDSample1.Domain.Families.Family", b =>
                {
                    b.Property<string>("Id")
                        .HasColumnType("nvarchar(450)");

                    b.Property<bool>("Active")
                        .HasColumnType("bit");

                    b.Property<string>("Description")
                        .HasColumnType("nvarchar(max)");

                    b.HasKey("Id");

                    b.ToTable("Families");
                });

            modelBuilder.Entity("DDDSample1.Domain.Players.Player", b =>
                {
                    b.Property<string>("Id")
                        .HasColumnType("nvarchar(450)");

                    b.Property<bool>("Active")
                        .HasColumnType("bit");

                    b.HasKey("Id");

                    b.ToTable("Players");
                });

            modelBuilder.Entity("DDDSample1.Domain.Products.Product", b =>
                {
                    b.Property<string>("Id")
                        .HasColumnType("nvarchar(450)");

                    b.Property<bool>("Active")
                        .HasColumnType("bit");

                    b.Property<string>("CategoryId")
                        .HasColumnType("nvarchar(max)");

                    b.Property<string>("Description")
                        .HasColumnType("nvarchar(max)");

                    b.HasKey("Id");

                    b.ToTable("Products");
                });

            modelBuilder.Entity("DDDNetCore.Domain.ConnectionRequests.DirectRequest", b =>
                {
                    b.OwnsOne("DDDNetCore.Domain.ConnectionRequests.ConnectionRequestStatus", "CurrentStatus", b1 =>
                        {
                            b1.Property<string>("DirectRequestId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<int>("CurrentStatus")
                                .HasColumnType("int");

                            b1.HasKey("DirectRequestId");

                            b1.ToTable("DirectRequests");

                            b1.WithOwner()
                                .HasForeignKey("DirectRequestId");
                        });

                    b.OwnsOne("DDDNetCore.Domain.Shared.ConnectionStrength", "Strength", b1 =>
                        {
                            b1.Property<string>("DirectRequestId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<int>("Strength")
                                .HasColumnType("int");

                            b1.HasKey("DirectRequestId");

                            b1.ToTable("DirectRequests");

                            b1.WithOwner()
                                .HasForeignKey("DirectRequestId");
                        });

                    b.OwnsOne("DDDNetCore.Domain.Shared.Message", "PlayerToTargetMessage", b1 =>
                        {
                            b1.Property<string>("DirectRequestId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<string>("Text")
                                .IsRequired()
                                .HasMaxLength(1000)
                                .HasColumnType("nvarchar(1000)");

                            b1.HasKey("DirectRequestId");

                            b1.ToTable("DirectRequests");

                            b1.WithOwner()
                                .HasForeignKey("DirectRequestId");
                        });

                    b.OwnsMany("DDDNetCore.Domain.Shared.Tag", "Tags", b1 =>
                        {
                            b1.Property<string>("DirectRequestId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<int>("Id")
                                .ValueGeneratedOnAdd()
                                .HasColumnType("int")
                                .UseIdentityColumn();

                            b1.Property<string>("tagName")
                                .IsRequired()
                                .HasMaxLength(50)
                                .HasColumnType("nvarchar(50)");

                            b1.HasKey("DirectRequestId", "Id");

                            b1.ToTable("DirectRequests_Tags");

                            b1.WithOwner()
                                .HasForeignKey("DirectRequestId");
                        });

                    b.Navigation("CurrentStatus")
                        .IsRequired();

                    b.Navigation("PlayerToTargetMessage")
                        .IsRequired();

                    b.Navigation("Strength")
                        .IsRequired();

                    b.Navigation("Tags");
                });

            modelBuilder.Entity("DDDNetCore.Domain.ConnectionRequests.IntroductionRequest", b =>
                {
                    b.OwnsOne("DDDNetCore.Domain.ConnectionRequests.ConnectionRequestStatus", "CurrentStatus", b1 =>
                        {
                            b1.Property<string>("IntroductionRequestId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<int>("CurrentStatus")
                                .HasColumnType("int");

                            b1.HasKey("IntroductionRequestId");

                            b1.ToTable("IntroductionRequests");

                            b1.WithOwner()
                                .HasForeignKey("IntroductionRequestId");
                        });

                    b.OwnsOne("DDDNetCore.Domain.Shared.ConnectionStrength", "Strength", b1 =>
                        {
                            b1.Property<string>("IntroductionRequestId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<int>("Strength")
                                .HasColumnType("int");

                            b1.HasKey("IntroductionRequestId");

                            b1.ToTable("IntroductionRequests");

                            b1.WithOwner()
                                .HasForeignKey("IntroductionRequestId");
                        });

                    b.OwnsOne("DDDNetCore.Domain.Shared.Message", "MiddleManToTargetMessage", b1 =>
                        {
                            b1.Property<string>("IntroductionRequestId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<string>("Text")
                                .IsRequired()
                                .HasMaxLength(1000)
                                .HasColumnType("nvarchar(1000)");

                            b1.HasKey("IntroductionRequestId");

                            b1.ToTable("IntroductionRequests");

                            b1.WithOwner()
                                .HasForeignKey("IntroductionRequestId");
                        });

                    b.OwnsOne("DDDNetCore.Domain.Shared.Message", "PlayerToMiddleManMessage", b1 =>
                        {
                            b1.Property<string>("IntroductionRequestId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<string>("Text")
                                .IsRequired()
                                .HasMaxLength(1000)
                                .HasColumnType("nvarchar(1000)");

                            b1.HasKey("IntroductionRequestId");

                            b1.ToTable("IntroductionRequests");

                            b1.WithOwner()
                                .HasForeignKey("IntroductionRequestId");
                        });

                    b.OwnsOne("DDDNetCore.Domain.Shared.Message", "PlayerToTargetMessage", b1 =>
                        {
                            b1.Property<string>("IntroductionRequestId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<string>("Text")
                                .IsRequired()
                                .HasMaxLength(1000)
                                .HasColumnType("nvarchar(1000)");

                            b1.HasKey("IntroductionRequestId");

                            b1.ToTable("IntroductionRequests");

                            b1.WithOwner()
                                .HasForeignKey("IntroductionRequestId");
                        });

                    b.OwnsMany("DDDNetCore.Domain.Shared.Tag", "Tags", b1 =>
                        {
                            b1.Property<string>("IntroductionRequestId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<int>("Id")
                                .ValueGeneratedOnAdd()
                                .HasColumnType("int")
                                .UseIdentityColumn();

                            b1.Property<string>("tagName")
                                .IsRequired()
                                .HasMaxLength(50)
                                .HasColumnType("nvarchar(50)");

                            b1.HasKey("IntroductionRequestId", "Id");

                            b1.ToTable("IntroductionRequests_Tags");

                            b1.WithOwner()
                                .HasForeignKey("IntroductionRequestId");
                        });

                    b.Navigation("CurrentStatus")
                        .IsRequired();

                    b.Navigation("MiddleManToTargetMessage")
                        .IsRequired();

                    b.Navigation("PlayerToMiddleManMessage")
                        .IsRequired();

                    b.Navigation("PlayerToTargetMessage")
                        .IsRequired();

                    b.Navigation("Strength")
                        .IsRequired();

                    b.Navigation("Tags");
                });

            modelBuilder.Entity("DDDNetCore.Domain.Connections.Connection", b =>
                {
                    b.OwnsOne("DDDNetCore.Domain.Shared.ConnectionStrength", "ConnectionStrength", b1 =>
                        {
                            b1.Property<string>("ConnectionId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<int>("Strength")
                                .HasColumnType("int");

                            b1.HasKey("ConnectionId");

                            b1.ToTable("Connections");

                            b1.WithOwner()
                                .HasForeignKey("ConnectionId");
                        });

                    b.OwnsMany("DDDNetCore.Domain.Shared.Tag", "Tags", b1 =>
                        {
                            b1.Property<string>("ConnectionId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<int>("Id")
                                .ValueGeneratedOnAdd()
                                .HasColumnType("int")
                                .UseIdentityColumn();

                            b1.Property<string>("tagName")
                                .IsRequired()
                                .HasMaxLength(50)
                                .HasColumnType("nvarchar(50)");

                            b1.HasKey("ConnectionId", "Id");

                            b1.ToTable("Connections_Tags");

                            b1.WithOwner()
                                .HasForeignKey("ConnectionId");
                        });

                    b.Navigation("ConnectionStrength")
                        .IsRequired();

                    b.Navigation("Tags");
                });

            modelBuilder.Entity("DDDNetCore.Domain.Missions.Mission", b =>
                {
                    b.OwnsOne("DDDNetCore.Domain.Missions.MissionDifficulty", "Difficulty", b1 =>
                        {
                            b1.Property<string>("MissionId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<int>("Difficulty")
                                .HasColumnType("int");

                            b1.HasKey("MissionId");

                            b1.ToTable("Missions");

                            b1.WithOwner()
                                .HasForeignKey("MissionId");
                        });

                    b.OwnsOne("DDDNetCore.Domain.Missions.MissionStatus", "CurrentStatus", b1 =>
                        {
                            b1.Property<string>("MissionId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<int>("CurrentStatus")
                                .HasColumnType("int");

                            b1.HasKey("MissionId");

                            b1.ToTable("Missions");

                            b1.WithOwner()
                                .HasForeignKey("MissionId");
                        });

                    b.Navigation("CurrentStatus")
                        .IsRequired();

                    b.Navigation("Difficulty")
                        .IsRequired();
                });

            modelBuilder.Entity("DDDSample1.Domain.Players.Player", b =>
                {
                    b.OwnsOne("DDDSample1.Domain.Players.PlayerDateOfBirth", "DateOfBirth", b1 =>
                        {
                            b1.Property<string>("PlayerId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<DateTime>("date")
                                .HasColumnType("datetime2");

                            b1.HasKey("PlayerId");

                            b1.ToTable("Players");

                            b1.WithOwner()
                                .HasForeignKey("PlayerId");
                        });

                    b.OwnsOne("DDDSample1.Domain.Players.PlayerEmail", "Email", b1 =>
                        {
                            b1.Property<string>("PlayerId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<string>("address")
                                .HasColumnType("nvarchar(max)");

                            b1.HasKey("PlayerId");

                            b1.ToTable("Players");

                            b1.WithOwner()
                                .HasForeignKey("PlayerId");
                        });

                    b.OwnsOne("DDDSample1.Domain.Players.PlayerEmotionalStatus", "EmotionalStatus", b1 =>
                        {
                            b1.Property<string>("PlayerId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<string>("Status")
                                .HasColumnType("nvarchar(max)");

                            b1.HasKey("PlayerId");

                            b1.ToTable("Players");

                            b1.WithOwner()
                                .HasForeignKey("PlayerId");
                        });

                    b.OwnsOne("DDDSample1.Domain.Players.PlayerFacebook", "Facebook", b1 =>
                        {
                            b1.Property<string>("PlayerId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<string>("Url")
                                .HasColumnType("nvarchar(max)");

                            b1.HasKey("PlayerId");

                            b1.ToTable("Players");

                            b1.WithOwner()
                                .HasForeignKey("PlayerId");
                        });

                    b.OwnsOne("DDDSample1.Domain.Players.PlayerLinkedIn", "LinkedIn", b1 =>
                        {
                            b1.Property<string>("PlayerId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<string>("Url")
                                .HasColumnType("nvarchar(max)");

                            b1.HasKey("PlayerId");

                            b1.ToTable("Players");

                            b1.WithOwner()
                                .HasForeignKey("PlayerId");
                        });

                    b.OwnsOne("DDDSample1.Domain.Players.PlayerName", "Name", b1 =>
                        {
                            b1.Property<string>("PlayerId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<string>("name")
                                .HasColumnType("nvarchar(max)");

                            b1.HasKey("PlayerId");

                            b1.ToTable("Players");

                            b1.WithOwner()
                                .HasForeignKey("PlayerId");
                        });

                    b.OwnsOne("DDDSample1.Domain.Players.PlayerPhoneNumber", "PhoneNumber", b1 =>
                        {
                            b1.Property<string>("PlayerId")
                                .HasColumnType("nvarchar(450)");

                            b1.Property<double>("phoneNumber")
                                .HasColumnType("float");

                            b1.HasKey("PlayerId");

                            b1.ToTable("Players");

                            b1.WithOwner()
                                .HasForeignKey("PlayerId");
                        });

                    b.Navigation("DateOfBirth");

                    b.Navigation("Email");

                    b.Navigation("EmotionalStatus");

                    b.Navigation("Facebook");

                    b.Navigation("LinkedIn");

                    b.Navigation("Name");

                    b.Navigation("PhoneNumber");
                });
#pragma warning restore 612, 618
        }
    }
}
